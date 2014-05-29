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
    int userID;
    char username[50];
} userInfo;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    userInfo *array;
} userArray;

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    double distance;
} competitorsDistance;

typedef struct __attribute__ ((packed)) {
    uint32_t userId;
    uint32_t time;
    uint32_t winnerId;
    uint32_t distance;
    double averageSpeed;
    uint32_t state;
} matchStats;


typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char type[2];
    matchStats *array;
} matchStatsHead;


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

/**
 This method gets stats from the past races
 @method getHomeStats
 
 */
+(homeStats *)getHomeStats;
/**
 This method sends race requests to opponents
 @method sendRequest
 
 
 */
+(void)sendRequest;
/**
 This method gets the requests from the server
 @method getRequest
 
 */
+(requestLookUpResult *)getRequests:(int)type1 type2:(int)type2;
/**
 This method sends an accept to the server 
 @method acceptRequest
 @param requestId The ID of the accepted request
 */
+(int)acceptRequest:(uint32_t) requestId;
/**
 This method send a package telling the server to cancel the request
 @method cancelMethod
 @param requestId The ID of the declined request
 */
+(int)cancelRequest:(uint32_t) requestId;

/**
 This method is used when a user wants to send a race request
 @method makeRequest
 @param userId the Id of the user you want to challenge
 @param distance The distance you want to challenge the opponent with
 
 */
+(int)makeRequest:(uint32_t) userId distance:(uint32_t)distance;
/**
 This method lets the user to searh for other users
 @method searhForUsers
 @param username The name of the opponent the user wants to find
 */


+(userArray *)searchForUsers:(NSString *)username;
/**
 This method starts to send the data to the server when a race starts
 @method startRace 
 @param reqID the ID of the user thats starts to run
 
 */

+(int)startRace:(int)reqID;
/**
 This sends the coordinates to the server
 @method
 @param latitude 
 @param longitude
 
 */

+(void)sendUpdatedCoordinates:(double)latitude longitude:(double)longitude;

/**
 This method is called when a user wants to quit the race
@method quitRace
 */

+(void)quitRace;
/**
 This method gets the distance that the competitors has gone from the server
 @method requestCompetitorDistance
 */

+(competitorsDistance *)requestCompetitorsDistance;

+(matchStatsHead *)getMatchStats;

+(void) sendGetHistory;
@end
