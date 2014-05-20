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
    
    //self.inputStream = inputStream;
    //self.outputStream = outputStream;
    
}
+(void) readStream:(uint8_t *) buffer maxLength:(int)maxLength {
    int bytesRead = 0;
    char tmpBuffer[maxLength];
    memset(tmpBuffer, 0, sizeof(char)*maxLength);
    
    while((bytesRead += [inputStream read:(uint8_t *)&tmpBuffer[bytesRead] maxLength:maxLength]) < maxLength){
    
    }
    //NSLog(@"Bytes read %d", bytesRead);
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



+(void *)getHomeStats {
    
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

+(void *)getRequests:(int)type1 type2:(int)type2 {

    requestLookUpResult *result = malloc(sizeof(requestLookUpResult));
    memset(result, 0, sizeof(requestLookUpResult));
    
    
    [self readStream:(uint8_t *)&result->requestLookUpMeta.length maxLength:sizeof(int)];
    [self readStream:(uint8_t *)&result->requestLookUpMeta.type maxLength:2];
    
    if ((result->requestLookUpMeta.type[0] == type1) && (result->requestLookUpMeta.type[1] == type2)) {
        
        result->requestLookUpMeta.length = ntohl(result->requestLookUpMeta.length)-2;
        double numberOfPackages = (double)(result->requestLookUpMeta.length)/(double)(sizeof(requestLookUp));
         NSLog(@"numberOfPackages: %f", numberOfPackages);
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
    
    return [outputStream write:(uint8_t *)&packet maxLength:sizeof(requestAccept)];

}

+(int)cancelRequest:(uint32_t) requestId{

    requestCancel packet;
    packet.length = CFSwapInt32HostToBig(6);
    packet.type[0] = 2;
    packet.type[1] = 3;
    packet.msg = requestId;
    
    return [outputStream write:(uint8_t *)&packet maxLength:sizeof(requestCancel)];


}


+(int)makeRequest:(uint32_t) userId distance:(uint32_t)distance {

    requestMake packet;
    packet.length = CFSwapInt32HostToBig(10);
    packet.type[0] = 2;
    packet.type[1] = 0;
    packet.userId = userId;
    packet.distance = distance;
    
    return [outputStream write:(uint8_t *)&packet maxLength:sizeof(requestMake)];

}

+(void *)searchForUsers {
    
    NSMutableArray *usersData = [[NSMutableArray alloc] init];
    
    uint32_t myInt32Value = 2;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    requestStats packet;
    packet.length = myInt32AsABigEndianNumber;
    packet.type[0] = 5;
    packet.type[1] = 0;
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(packet)];
    
    users *result = malloc(sizeof(users));
    
    [self readStream:(uint8_t *)&result maxLength:sizeof(result)];
    
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
        return 1;
    } else {
        NSLog (@"not succesful");
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

@end
