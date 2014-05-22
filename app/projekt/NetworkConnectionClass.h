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

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    char username[50];
    double averageSpeed;
    double averageDistance;
    uint32_t wins;
    uint32_t matches;
    uint32_t request;
} homeStats;

typedef struct __attribute__ ((packed)) {
    int requestID;
    int userID;
    char username[50];
    int date[6];
    int state;
    int distance;
} requestLookUp;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
} requestLookUpMeta;

typedef struct __attribute__ ((packed)) {
    requestLookUpMeta requestLookUpMeta;
    requestLookUp *requestLookUp;
} requestLookUpResult;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    struct user *userArray;
} userArray;

typedef struct __attribute__ ((packed)) {
    int userID;
    char username[50];
} user;


@interface NetworkConnectionClass : NSObject <NSStreamDelegate>

@property (nonatomic) homeStats result;



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


/**
 This method is used when a user wants to sign out from the app
 @method signOut 
 @param no parameters
 */
+(void)signOut;

+(void *)getHomeStats;

+(void)sendRequest;
+(void *)getRequests:(int)type1 type2:(int)type2;

+(int) acceptRequest:(uint32_t) requestId;
+(int) cancelRequest:(uint32_t) requestId;

+(int)makeRequest:(uint32_t) userId distance:(uint32_t)distance;

+(void *)searchForUsers:(NSString *)username;

+(int)startRace:(int)reqID;

+(void)sendUpdatedCoordinates:(double)latitude longitude:(double)longitude;

+(void)quitRace;

@end
