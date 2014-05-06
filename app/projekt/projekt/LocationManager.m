//
//  projekt.m
//  projekt
//
//  Created by Joel Sandberg on 2014-05-05.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "LocationManager.h"

@implementation CLLocationManagerDelegate

@synthesize locationManager, currentLocation;


- (void)locationManager:(CLLocationManager *)manager didUpdateToLocation:(CLLocation *)newLocation fromLocation:(CLLocation *)oldLocation {
    
    self.currentLocation = newLocation;
    if(newLocation.horizontalAccuracy <= 100.0f)
        { [locationManager stopUpdatingLocation]; }



}


- (void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error
{
    if(error.code == kCLErrorDenied) {
        [locationManager stopUpdatingLocation];
    } else if(error.code == kCLErrorLocationUnknown) {
        // retry
    } else {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error retrieving location"
                                                        message:[error description]
                                                       delegate:nil
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        [alert show];
    }
}




@end
