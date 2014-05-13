//
//  RaceViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 08/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "RaceViewController.h"

@interface RaceViewController ()

@property (nonatomic , strong) CLLocationManager *locationManager;
@property CLLocationCoordinate2D myCoordinate;
@property (weak, nonatomic) IBOutlet MKMapView *mapView;
@property (nonatomic) CLLocation *firstPosition;
@property (nonatomic) CLLocationCoordinate2D previousPosition;
@property (nonatomic, retain) MKPolyline *routeLine; //your line
@property (nonatomic, retain) MKPolylineView *routeLineView; //overlay view
@property (nonatomic, retain) NSMutableArray* points;
@property (weak, nonatomic) IBOutlet UISlider *yourSlider;
@property (weak, nonatomic) IBOutlet UISlider *competitorSlider;
@property (weak, nonatomic) IBOutlet UILabel *competitorName;
@property (nonatomic) double totalDistance;
@property (nonatomic) double totalCompetitorDistance;
@property (weak, nonatomic) IBOutlet UILabel *toGoalLabel;
@property (weak, nonatomic) IBOutlet UILabel *betweenPlayersLabel;
@property (nonatomic) NSInteger distance;
@property (weak, nonatomic) IBOutlet UILabel *differenceLabel;

@end

@implementation RaceViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}


- (void)viewDidLoad
{
    
    [self performSegueWithIdentifier:@"startCountdown" sender:self];
    [super viewDidLoad];
    
    NSOperationQueue *myQueue = [[NSOperationQueue alloc] init];
    [myQueue addOperationWithBlock:^{
        
        
        self.mapView.delegate = self;
        //self.mapView.userInteractionEnabled=NO;
        self.mapView.showsUserLocation=YES;
        self.mapView.tintColor = [UIColor blackColor];
        
        [[NSOperationQueue mainQueue] addOperationWithBlock:^{
            
            // if location services are on
            if([CLLocationManager locationServicesEnabled])
            {
                // if location services are restricted do nothing
                if ([CLLocationManager authorizationStatus] == kCLAuthorizationStatusDenied ||
                    [CLLocationManager authorizationStatus] == kCLAuthorizationStatusRestricted )
                {
                    NSLog(@"Location tracking disabled.");
                }
                else
                {
                    NSLog(@"all ok");
                    self.locationManager = [[CLLocationManager alloc] init];
                    self.locationManager.delegate = self;
                    [self.locationManager setDistanceFilter:kCLDistanceFilterNone];
                    [self.locationManager setDesiredAccuracy:kCLLocationAccuracyHundredMeters];
                    [self.locationManager startUpdatingLocation];
                    NSLog(@"latitude= %f longitude = %f",self.locationManager.location.coordinate.latitude, self.locationManager.location.coordinate.latitude);
                }
            }
            
            [self.mapView setVisibleMapRect:[self.routeLine boundingMapRect]]; //If you want the route to be visible
            
            self.routeLineView = [[MKPolylineView alloc] init];
            self.routeLine = [[MKPolyline alloc] init];
            [self.mapView addOverlay:self.routeLine];
           
            
            
            // Do any additional setup after loading the view.
            /*
             UIBezierPath *path1 = [UIBezierPath bezierPath];
             [path1 moveToPoint:CGPointMake(10,10)];
             [path1 addLineToPoint:CGPointMake(70,10)];
             [path1 stroke];
             
             
             
             
             UIBezierPath *path2 = [UIBezierPath bezierPath];
             [path2 moveToPoint:CGPointMake(70,10)];
             [path2 addLineToPoint:CGPointMake(140,10)];
             [path2 stroke];
             
             
             UIColor *green = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
             UIGraphicsBeginImageContext(CGSizeMake(120, 120));
             [[UIColor blackColor] setStroke];
             path1.lineCapStyle = kCGLineCapRound;
             path1.lineWidth = 15.0f;
             [green setStroke];
             [path1 stroke];
             
             UIColor *red = [UIColor colorWithRed:0.91 green:0.04 blue:0.09 alpha:1];
             path2.lineWidth = 15.0f;
             path2.lineCapStyle = kCGLineCapRound;
             [red setStroke];
             [path2 stroke];
             self.drawpad.image = UIGraphicsGetImageFromCurrentImageContext();
             
             //[self drawRect:CGRectMake(0, 0, 100, 100)];
             
             UIGraphicsEndImageContext();
             */
        }];
    }];
    
    _distance = 5000;
    _yourSlider.minimumValue = 0;
    _yourSlider.maximumValue = _distance;
    
    _competitorSlider.minimumValue = 0;
    _competitorSlider.maximumValue = _distance;
    
    [_yourSlider setThumbImage:[UIImage imageNamed:@"slider"]
                                forState:UIControlStateNormal];
    [_competitorSlider setThumbImage:[UIImage imageNamed:@"slider"]
                      forState:UIControlStateNormal];
    _totalCompetitorDistance = 2500;
    _competitorSlider.value = _totalCompetitorDistance;
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}
- (void)mapView:(MKMapView *)mapView didUpdateUserLocation:(MKUserLocation *)userLocation {
    
    
    float spanX = fabs(_mapView.userLocation.coordinate.longitude - (_firstPosition.coordinate.longitude))*1.6;
    float spanY = fabs(_mapView.userLocation.coordinate.latitude - (_firstPosition.coordinate.latitude))*2.5;
    
    MKCoordinateRegion region;
    region.center.latitude =
    ((_mapView.userLocation.coordinate.latitude) + (_firstPosition.coordinate.latitude))/2;
    
    region.center.longitude =
    ((_mapView.userLocation.coordinate.longitude) + (_firstPosition.coordinate.longitude))/2;
    
    region.span.latitudeDelta = spanY;
    region.span.longitudeDelta = spanX;
    
    if (_previousPosition.latitude == 0) {
        _previousPosition.latitude = self.mapView.userLocation.coordinate.latitude;
        _previousPosition.longitude = self.mapView.userLocation.coordinate.longitude;
    }
    
    
    CLLocation *prev = [[CLLocation alloc] initWithLatitude:_previousPosition.latitude longitude:_previousPosition.longitude];
    [self calculateDistance];
    
    [self.mapView setRegion:[self.mapView regionThatFits:region] animated:YES];
    [self.points addObject:self.mapView.userLocation];
    
    //NSLog(@"acc: %f", self.mapView.userLocation.location.horizontalAccuracy);
    if (_firstPosition != nil) {
        [self drawRoute:@[self.mapView.userLocation, prev]];
    }
    
    
    
    _previousPosition.longitude = self.mapView.userLocation.location.coordinate.longitude;
    _previousPosition.latitude = self.mapView.userLocation.location.coordinate.latitude;
    
    
    
}

- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations {
    // if ((_firstPosition.coordinate.latitude == 0) && (_firstPosition.coordinate.longitude == 0))  {
    
    
    if (_firstPosition == nil) {
        NSLog(@"%f", self.mapView.userLocation.location.horizontalAccuracy);
        if (self.mapView.userLocation.location.horizontalAccuracy < 0)
        {
            // No Signal
        } else if (self.mapView.userLocation.location.horizontalAccuracy > 163) {
            // Poor Signal
        } else if (self.mapView.userLocation.location.horizontalAccuracy > 48) {
            // Average Signal
            _firstPosition = [[CLLocation alloc] initWithLatitude:self.locationManager.location.coordinate.latitude longitude:self.locationManager.location.coordinate.longitude];
        } else {
            _firstPosition = [[CLLocation alloc] initWithLatitude:self.locationManager.location.coordinate.latitude longitude:self.locationManager.location.coordinate.longitude];
            
        }
    }
    
    [self mapView:self.mapView didUpdateUserLocation:self.mapView.userLocation];
    [NetworkConnectionClass sendUpdatedCoordinates];
    
}

- (MKOverlayView *)mapView:(MKMapView *)mapView viewForOverlay:(id <MKOverlay>)overlay {
    MKPolylineView *polylineView = [[MKPolylineView alloc] initWithPolyline:overlay];
    polylineView.strokeColor = [UIColor redColor];
    polylineView.lineWidth = 10.0;
    polylineView.lineCap = kCGLineCapRound;
    polylineView.alpha = 1;
    
    return polylineView;
}

- (void)drawRoute:(NSArray *) path {
    NSInteger numberOfSteps = path.count;
    CLLocationCoordinate2D coordinates[numberOfSteps];
    
    for (NSInteger index = 0; index < numberOfSteps; index++) {
        CLLocation *location = [path objectAtIndex:index];
        CLLocationCoordinate2D coordinate = location.coordinate;
        
        coordinates[index] = coordinate;
    }
    
    MKPolyline *polyLine = [MKPolyline polylineWithCoordinates:coordinates count:numberOfSteps];
    
    [self.mapView addOverlay:polyLine];
    
}

-(void)calculateDistance {
    
    if ((self.mapView.userLocation != nil) && (_previousPosition.latitude != 0)) {
        
        double lat1rad = _previousPosition.latitude * M_PI/180;
        double lon1rad = _previousPosition.longitude * M_PI/180;
        double lat2rad = self.mapView.userLocation.coordinate.latitude * M_PI/180;
        double lon2rad = self.mapView.userLocation.coordinate.longitude * M_PI/180;
        
        //deltas
        double dLat = lat2rad - lat1rad;
        double dLon = lon2rad - lon1rad;
        
        double a = sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat1rad) * cos(lat2rad);
        double c = 2 * asin(sqrt(a));
        double R = 6372.8;
        
        double prevTotalDistance = _totalDistance;

        _totalDistance = (R * c * 1000) + prevTotalDistance;
        _yourSlider.value = _totalDistance;
        _toGoalLabel.text = [NSString stringWithFormat:@"%0.1f", (_distance - _totalDistance)/1000];
        _betweenPlayersLabel.text = [NSString stringWithFormat:@"%ld", (long)_distance];
        _differenceLabel.text = [NSString stringWithFormat:@"%0.1f km", (_totalDistance - _totalCompetitorDistance)/1000];
        if ((_totalDistance - _totalCompetitorDistance) < 0) {
            _differenceLabel.textColor = [UIColor colorWithRed:0.91 green:0.04 blue:0.09 alpha:1];
        } else if ((_totalDistance - _totalCompetitorDistance) > 0) {
            _differenceLabel.textColor = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
        }
        

    }
}



@end