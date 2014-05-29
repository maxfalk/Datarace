//
//  RaceViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 08/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreLocation/CoreLocation.h>
#import <MapKit/MapKit.h>
#import "NetworkConnectionClass.h"
#import "FinishlineViewController.h"

@interface RaceViewController : UIViewController <CLLocationManagerDelegate, MKMapViewDelegate>
@property int reqID;
@property (nonatomic) NSInteger distance;
@property (nonatomic, strong) NSMutableArray *points;

@end
