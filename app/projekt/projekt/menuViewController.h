//
//  menuViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NetworkConnectionClass.h"
#import "RaceViewController.h"
#import "FinishlineViewController.h"

@interface menuViewController : UIViewController
{
    UIImageView *drawpad;
}
@property (retain, nonatomic) IBOutlet UIImageView *drawpad;

@end
