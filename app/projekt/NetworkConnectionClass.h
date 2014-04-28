//
//  NetworkConnectionClass.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 25/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NetworkConnectionClass : NSObject <NSStreamDelegate>
{
    //NSInputStream *inputStream;
    //NSOutputStream *outputStream;
    //CFReadStreamRef readStream;
    //CFWriteStreamRef writeStream;
}


//@property (strong) NSInputStream *inputStream;
//@property (strong) NSOutputStream *outputStream;

+(void)initNetworkCommunication;
+(int)sendLoginPackage:(NSString *)username password:(NSString *)password;
+(int)signupUser:(NSString *)username password:(NSString *)password email:(NSString *)email;

@end
