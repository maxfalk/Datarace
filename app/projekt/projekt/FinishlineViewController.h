//
//  FinishlineViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 26/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <MapKit/MapKit.h>
#import "NetworkConnectionClass.h"

@interface FinishlineViewController : UIViewController <MKMapViewDelegate>

@property (nonatomic, strong) NSMutableArray *coordinates;
@property (nonatomic, retain) NSArray* pts;
@property int distance;
@property (strong, nonatomic) NSString *timeString;
@property (strong, nonatomic) NSString *avgSpeed;
@property (strong, nonatomic) NSString *header;

@end
