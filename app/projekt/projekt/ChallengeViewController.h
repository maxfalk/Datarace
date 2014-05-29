//
//  ChallengeViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 22/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NetworkConnectionClass.h"
#import "CWStatusBarNotification.h"


@interface ChallengeViewController : UIViewController

//@property (nonatomic) NSInputStream *inputStream;
//@property (nonatomic) NSOutputStream *outputStream;

@property (weak, nonatomic) IBOutlet UILabel *distanceLabel;
@property (strong, nonatomic) IBOutlet UILabel *challenger;
@property (strong, nonatomic) NSString *challengerUsername;
@property int challengerID;

@property (weak, nonatomic) IBOutlet UILabel *updateDistanceLabel;
@property (weak, nonatomic) IBOutlet UISlider *updateDistanceSlider;

@property (strong, nonatomic) CWStatusBarNotification *notification;


@end
