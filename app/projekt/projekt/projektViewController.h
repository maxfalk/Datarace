//
//  projektViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreLocation/CoreLocation.h>
#import "NetworkConnectionClass.h"
#import "SignupViewController.h"


@interface projektViewController : UIViewController  <UITextFieldDelegate, UIAlertViewDelegate>
{
   // CLLocationManager *locationManager;
}
@property (weak, nonatomic) IBOutlet UIButton *loginButton;
@property (strong) NetworkConnectionClass *networkConnection;

@end
