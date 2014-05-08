//
//  NetworkConnectionClass.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 25/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//
/**
 
@class: NetworkConnectionClass

 

*/
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


/**
 @method: initNetworkCommunication
 Initiates a networkcommunication
 */
+(void)initNetworkCommunication;

/**
 @method: sendLoginPackage sends the login package to the server.
 @param username The username that wants to log in
 @param password The password
 */
+(int)sendLoginPackage:(NSString *)username password:(NSString *)password;

/**
 This method is used when you want to sign up a new user
 
 @method signupUser
 @param username The name that the user wants to have
 @param password The password that the user wants
 @param email    The email The users e-mail adress
 
 
 
 */
+(int)signupUser:(NSString *)username password:(NSString *)password email:(NSString *)email;

+(void)signOut;

+(void)sendUpdatedCoordinates;

@end
