//
//  SignupViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 25/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "projektViewController.h"
#import "NetworkConnectionClass.h"

@interface SignupViewController : UIViewController <UIScrollViewDelegate, UITextFieldDelegate>

@property (nonatomic, copy) projektViewController *networkConnection;

@end
