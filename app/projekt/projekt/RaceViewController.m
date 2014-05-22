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
@property int check;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *quitButton;
@property (weak, nonatomic) IBOutlet UILabel *timeLabel;
@property (nonatomic) NSDate *now;
@property (weak, nonatomic) NSTimer *stopWatch;
@property int currentTimeInSeconds;
@property (weak, nonatomic) IBOutlet UILabel *averageSpeedLabel;


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


- (void)viewDidLoad {
    
    
    [self performSegueWithIdentifier:@"startCountdown" sender:self];
    [super viewDidLoad];
    
    _check = 0;
    int result = [NetworkConnectionClass startRace:_reqID];
    if (result == 1) {
        NSLog(@"success");
    } else {
        NSLog (@"not succesful");
    }
    
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
                    self.locationManager = [[CLLocationManager alloc] init];
                    self.locationManager.delegate = self;
                    [self.locationManager setDistanceFilter:kCLDistanceFilterNone];
                    [self.locationManager setDesiredAccuracy:kCLLocationAccuracyHundredMeters];
                    [self.locationManager startUpdatingLocation];
                }
            }
            
            [self.mapView setVisibleMapRect:[self.routeLine boundingMapRect]]; //If you want the route to be visible
            
            self.routeLineView = [[MKPolylineView alloc] init];
            self.routeLine = [[MKPolyline alloc] init];
            [self.mapView addOverlay:self.routeLine];
            
            
            
        }];
    }];
    
    _distance = 5000;
    _yourSlider.minimumValue = 0;
    _yourSlider.maximumValue = _distance;
    
    _competitorSlider.minimumValue = 0;
    _competitorSlider.maximumValue = _distance;
    
    UIImage *image = [UIImage imageNamed:@"slider"];
    [_yourSlider setThumbImage:image
                      forState:UIControlStateNormal];
    [_competitorSlider setThumbImage:image
                            forState:UIControlStateNormal];
    _totalCompetitorDistance = 2500;
    _competitorSlider.value = _totalCompetitorDistance;

    _averageSpeedLabel.text = @"Calculating...";
    
    
    
    //[self updateStopWatch];
    
}

-(void)viewDidAppear:(BOOL)animated {
    _stopWatch = [NSTimer scheduledTimerWithTimeInterval:(0.01) target:self selector:@selector(timerTicked:) userInfo:nil repeats:YES];

}

- (NSTimer *)createTimer {
    return [NSTimer scheduledTimerWithTimeInterval:1 target:self selector:@selector(timerTicked:) userInfo:nil repeats:YES];
}



- (void)timerTicked:(NSTimer *)timer {
    
    _currentTimeInSeconds++;
    _timeLabel.text = [self formattedTime:_currentTimeInSeconds];
    
    if ((_currentTimeInSeconds % 500) == 0) {
        float avrg = ((_totalDistance*1000)/(_currentTimeInSeconds*3.6));
    _averageSpeedLabel.text = [NSString stringWithFormat: @"%.0f km/h",avrg];
    }
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (NSString *)formattedTime:(int)totalMilliSeconds {
    
    int milliSeconds = (totalMilliSeconds % 100);
    int seconds = (totalMilliSeconds / 100) % 60;
    int minutes = (totalMilliSeconds / 6000) % 60;
    int hours = (totalMilliSeconds / 720000) % 60;
    
    if (hours == 0) {
        return [NSString stringWithFormat:@"%02d:%02d:%02d", minutes, seconds, milliSeconds];
    } else {
        return [NSString stringWithFormat:@"%02d:%02d:%02d:%02d",hours, minutes, seconds, milliSeconds];
    }
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
    
    if (_firstPosition != nil) {
        [self drawRoute:@[self.mapView.userLocation, prev]];
    }
    
    
    
    _previousPosition.longitude = self.mapView.userLocation.location.coordinate.longitude;
    _previousPosition.latitude = self.mapView.userLocation.location.coordinate.latitude;
    
    
    
}

- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations {
    if (_check == 0) {
        
        if (_firstPosition == nil) {
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
        [NetworkConnectionClass sendUpdatedCoordinates:_previousPosition.latitude longitude:_previousPosition.longitude];
    }
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

- (IBAction)quitButtonPressed:(id)sender {
    _check = 1;
    self.mapView.showsUserLocation = NO;
    [self.locationManager stopUpdatingLocation];
    [self.navigationController popViewControllerAnimated:YES];
    [_stopWatch invalidate];
    [NetworkConnectionClass quitRace];
}

-(void)updateStopWatch {
    
    _timeLabel.text = [NSString stringWithFormat:@"%@", _stopWatch];
}


@end