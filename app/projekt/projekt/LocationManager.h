//
//  projekt.h
//  projekt
//
//  Created by Joel Sandberg on 2014-05-05.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <CoreLocation/CoreLocation.h>

@interface CLLocationManagerDelegate : CLLocationManager

@property (strong, nonatomic) CLLocationManager *locationManager;
@property (strong, nonatomic) CLLocation *currentLocation;

@end
