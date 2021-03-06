//
//  NetworkConnectionClass.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 25/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//
#define PORT 8888
#define ADDRESS @"83.253.15.24"
#import "NetworkConnectionClass.h"
#import "sys/socket.h"

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type;
    char username [50];
    char password [50];
} login;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type;
    char username [50];
    char password [50];
    char email [50];
} signup;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type;
    char message;
} loginOutput;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
} requestStats;


typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    uint32_t msg;
} requestAccept;


typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    uint32_t msg;
} requestCancel;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    uint32_t userId;
    uint32_t distance;
} requestMake;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    uint32_t msg;
} match;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
} quitMatch;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    double longitude;
    double latitude;
} GPSCoord;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    char username[50];
} userSearch;

@implementation NetworkConnectionClass
//@synthesize inputStream, outputStream;

static NSInputStream *inputStream;
static NSOutputStream *outputStream;


+(void)initNetworkCommunication {
    
    CFReadStreamRef readStream;
    CFWriteStreamRef writeStream;
    
    CFStreamCreatePairWithSocketToHost(NULL, (CFStringRef)@"83.253.15.24", 8888, &readStream, &writeStream);
    inputStream = (__bridge NSInputStream *) readStream;
    outputStream = (__bridge NSOutputStream *) writeStream;
    
    [inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    
    [inputStream open];
    [outputStream open];
    
    
}
+(void)readStream:(uint8_t *) buffer maxLength:(int)maxLength {
    int bytesRead = 0;
    char tmpBuffer[maxLength];
    memset(tmpBuffer, 0, sizeof(char)*maxLength);
    
    while((bytesRead += [inputStream read:(uint8_t *)&tmpBuffer[bytesRead] maxLength:maxLength]) < maxLength){
        
    }
    memcpy(buffer, tmpBuffer, maxLength);
    
    
}


+(int)sendLoginPackage:(NSString *)username password:(NSString *)password {
    
    const char *user = [username UTF8String];
    const char *pass = [password UTF8String];
    
    //length
    uint32_t myInt32Value = 101;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    login packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type = 0;
    memset(packet.username, 0, 50);
    strcpy(packet.username, user);
    memset(packet.password, 0, 50);
    strcpy(packet.password, pass);
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(login)];
    
    //receive message
    loginOutput result;
    result.type = (char)3;
    [inputStream read:(uint8_t *)&result maxLength:sizeof(result)];
    
    if (result.type == (char)0)  {
        if (result.message == (char)0) {
            NSLog(@"login successful");
            return 0;
        } else if (result.message == (char)1) {
            NSLog(@"wrong username");
            return 1;
        } else if (result.message == (char)2){
            NSLog(@"wrong password");
            return 2;
        }
    } else
        NSLog(@"wrong package");
    return 3;
}

+(int)signupUser:(NSString *)username password:(NSString *)password email:(NSString *)email {
    
    const char *user = [username UTF8String];
    const char *pass = [password UTF8String];
    const char *mail = [email UTF8String];
    
    //length
    uint32_t myInt32Value = 151;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    signup packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type = 1;
    memset(packet.username, 0, 50);
    strcpy(packet.username, user);
    memset(packet.password, 0, 50);
    strcpy(packet.password, pass);
    memset(packet.email, 0, 50);
    strcpy(packet.email, mail);
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(signup)];
    
    loginOutput result;
    result.type = 3;
    [inputStream read:(uint8_t *)&result maxLength:sizeof(result)];
    
    if (result.type == (char)1)  {
        if (result.message == (char)0) {
            NSLog(@"signup successful");
            return 0;
            
        } else if (result.message == (char)1) {
            NSLog(@"signup unsuccessful");
            return 1;
        }
        
    } else
        NSLog(@"wrong package");
    return 3;
}

+(void)signOut {
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    loginOutput packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type = 0;
    packet.message = (char)3;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(loginOutput)];
}

+(int)getNumberOfPendingRequests {
    
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    requestLookUpMeta packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 2;
    packet.type[1] = 6;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(requestLookUpMeta)];
    
    pendingRequests result;
    [inputStream read:(uint8_t *)&result maxLength:sizeof(pendingRequests)];

    return result.requests;
}

+(homeStats *)getHomeStats {
    
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    requestStats packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 3;
    packet.type[1] = 0;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(packet)];
    
    homeStats *result = malloc(sizeof(homeStats));
    [inputStream read:(uint8_t *)result maxLength:sizeof(*result)];
    
    return result;
}

+(void)sendRequest {
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    requestStats packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 2;
    packet.type[1] = 1;
    requestLookUpResult *result = malloc(sizeof(requestLookUpResult));
    memset(result, 0, sizeof(requestLookUpResult));
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(packet)];
}

+(requestLookUpResult *)getRequests:(int)type1 type2:(int)type2 {
    
    requestLookUpResult *result = malloc(sizeof(requestLookUpResult));
    memset(result, 0, sizeof(requestLookUpResult));
    
    
    [self readStream:(uint8_t *)&result->requestLookUpMeta.length maxLength:sizeof(int)];
    [self readStream:(uint8_t *)&result->requestLookUpMeta.type maxLength:2];
    
    if ((result->requestLookUpMeta.type[0] == type1) && (result->requestLookUpMeta.type[1] == type2)) {
        
        result->requestLookUpMeta.length = ntohl(result->requestLookUpMeta.length)-2;
        double numberOfPackages = (double)(result->requestLookUpMeta.length)/(double)(sizeof(requestLookUp));
        requestLookUp *reqLookUp = malloc(numberOfPackages*sizeof(requestLookUp));
        if(reqLookUp == nil){
            NSLog(@"Out of space");
            return 0;
        }
        memset(reqLookUp, 0, numberOfPackages*sizeof(requestLookUp));
        
        for(int i = 0; i < numberOfPackages; i++) {
            [self readStream:(uint8_t *)(reqLookUp+i) maxLength:sizeof(requestLookUp)];
            
        }
        result->requestLookUp = reqLookUp;
        
    }
    
    return result;
}
+(int)acceptRequest:(uint32_t) requestId {
    
    requestAccept packet;
    packet.length = CFSwapInt32HostToBig(6);
    packet.type[0] = 2;
    packet.type[1] = 2;
    packet.msg = requestId;
    
    return (int)[outputStream write:(uint8_t *)&packet maxLength:sizeof(requestAccept)];
    
}

+(int)cancelRequest:(uint32_t) requestId{
    
    requestCancel packet;
    packet.length = CFSwapInt32HostToBig(6);
    packet.type[0] = 2;
    packet.type[1] = 3;
    packet.msg = requestId;
    
    return (int)[outputStream write:(uint8_t *)&packet maxLength:sizeof(requestCancel)];
    
    
}


+(int)makeRequest:(uint32_t) userId distance:(uint32_t)distance {
    
    requestMake packet;
    packet.length = CFSwapInt32HostToBig(10);
    packet.type[0] = 2;
    packet.type[1] = 0;
    packet.userId = userId;
    packet.distance = distance;
    
    return (int)[outputStream write:(uint8_t *)&packet maxLength:sizeof(requestMake)];
    
}

+(userArray *)searchForUsers:(NSString *)username {
    
    uint32_t myInt32Value = 52;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    userSearch packet;
    
    const char *user = [username UTF8String];
    memset(packet.username, 0, 50);
    strcpy(packet.username, user);
    
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 5;
    packet.type[1] = 0;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(packet)];
    
    userArray *result = malloc(sizeof(userArray));
    memset(result, 0, sizeof(userArray));
    
    [inputStream read:(uint8_t *)result maxLength:6];
    
    if ((result->type[0] == 5) && (result->type[1] == 1)) {
        
        result->length = ntohl(result->length)-2;
        int numberOfPackages = ((result->length)/sizeof(userInfo));
        userInfo *user = malloc(numberOfPackages*sizeof(userInfo));
        for(int i = 0; i < numberOfPackages; i++) {
    
            [self readStream:(uint8_t *)(user+i) maxLength:sizeof(userInfo)];
            
            result->array = user;
        }
        
    }
    
    //result->array = user;
    
    return result;
}

+(int)startRace:(int)reqID {
    
    uint32_t myInt32Value = 6;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    match packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 4;
    packet.type[1] = 0;
    packet.msg = reqID;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(packet)];
    
    match *result = malloc(sizeof(match));
    [inputStream read:(uint8_t *)result maxLength:sizeof(*result)];
    
    if ((result->type[0] == 4) && (result->type[1] == 1)) {
        NSLog(@"success");
        free(result);
        return 1;
    } else {
        NSLog (@"not succesful");
        free(result);
        return 0;
    }
}

+(void)sendUpdatedCoordinates:(double)latitude longitude:(double)longitude {
    
    uint32_t myInt32Value = 18;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    GPSCoord packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 4;
    packet.type[1] = 2;
    packet.latitude = latitude;
    packet.longitude = longitude;
    
    if ((latitude != 0) && (longitude != 0)) {
        [outputStream write:(uint8_t *)&packet maxLength:sizeof(GPSCoord)];
    }
}

+(void)quitRace {
    
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    quitMatch packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 4;
    packet.type[1] = 3;
    
    [outputStream write:(uint8_t *)&packet maxLength:sizeof(quitMatch)];
}

+(competitorsDistance *)requestCompetitorsDistance {
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    requestLookUpMeta packet;

    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 4;
    packet.type[1] = 4;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(requestLookUpMeta)];
    
    competitorsDistance *result = malloc(sizeof(competitorsDistance));
    memset(result, 0, sizeof(competitorsDistance));
    
    [inputStream read:(uint8_t* )result maxLength:sizeof(competitorsDistance)];
    
    
        return result;
}

+(matchStatsHead *)getMatchStats {
    
    matchStatsHead *head = malloc(sizeof(matchStatsHead));
    
    [self readStream: (uint8_t *)head maxLength:6];
    head->length = ntohl(head->length)-2;
    int numberOfPackages = (head->length)/sizeof(matchStats);
    matchStats *stats = malloc(numberOfPackages*sizeof(matchStats));
    head->length = numberOfPackages;
    
    for (int i = 0; i < numberOfPackages; i++) {
        [self readStream:(uint8_t *)(stats+i) maxLength:sizeof(matchStats)];
    }
    
    head->array = stats;
    
    return head;
}

+(void)sendGetHistory{
    
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    requestStats packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 3;
    packet.type[1] = 2;
    [outputStream write:(uint8_t *)&packet maxLength:6];
    

}

+(void)sendGetResults{
    
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    requestStats packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 4;
    packet.type[1] = 3;
    [outputStream write:(uint8_t *)&packet maxLength:6];
    
    
}

@end
