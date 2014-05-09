//
//  NetworkConnectionClass.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 25/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "NetworkConnectionClass.h"

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
    
    //[inputStream setDelegate:self];
   //[outputStream setDelegate:self];
    [inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
   
    [inputStream open];
    [outputStream open];
    
    //self.inputStream = inputStream;
    //self.outputStream = outputStream;
    
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

+(void)sendUpdatedCoordinates {
    NSLog(@"UPDATE!");
}



@end
