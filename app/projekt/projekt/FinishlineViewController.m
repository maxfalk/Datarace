//
//  FinishlineViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 26/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "FinishlineViewController.h"

@interface FinishlineViewController ()
@property (strong, nonatomic) IBOutlet MKMapView *mapView;

@property (weak, nonatomic) IBOutlet UILabel *avgSpeedLabel;
@property (weak, nonatomic) IBOutlet UILabel *distanceLabel;
@property (weak, nonatomic) IBOutlet UILabel *timeLabel;
@property (weak, nonatomic) IBOutlet UIImageView *resultImageView;
@property (weak, nonatomic) IBOutlet UILabel *resultLabel;
@property (weak, nonatomic) IBOutlet UILabel *headerLabel;

@end

@implementation FinishlineViewController
@synthesize coordinates = _coordinates;

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
    //_coordinates = [[NSMutableArray alloc] init];
    [super viewDidLoad];
    [self.navigationItem setHidesBackButton:YES];
    _timeLabel.text = _timeString;
    if ([_avgSpeedLabel.text isEqual: @"Calculating..."]) {
        _avgSpeedLabel.text = @"0 km/h";
    } else {
        _avgSpeedLabel.text = _avgSpeed;
        
        
    }
    
    _headerLabel.text = _header;
    self.mapView.delegate = self;
    //self.mapView.userInteractionEnabled=NO;
    self.mapView.showsUserLocation=NO;
    self.mapView.tintColor = [UIColor blackColor];
    //self.mapView.camera =
    
    [self.mapView setShowsBuildings:YES];
    MKCoordinateRegion mapRegion;
    CLLocation *firstObject = [_coordinates objectAtIndex:0];
    CLLocation *lastObject = [_coordinates lastObject];
    
    MKPointAnnotation *first = [MKPointAnnotation alloc];
    first.coordinate = firstObject.coordinate;
    
    MKPointAnnotation *last;
    last.coordinate = lastObject.coordinate;
    
    NSLog(@"%f", last.coordinate.latitude);
    
    float spanX = fabs(firstObject.coordinate.longitude - lastObject.coordinate.longitude)*2;
    float spanY = fabs(firstObject.coordinate.latitude - lastObject.coordinate.latitude)*2.5;
    
    mapRegion.center.latitude =
    (firstObject.coordinate.latitude + lastObject.coordinate.latitude)/2;
    
    mapRegion.center.longitude =
    (firstObject.coordinate.longitude + lastObject.coordinate.longitude)/2;
    
    mapRegion.span.latitudeDelta = spanY;
    mapRegion.span.longitudeDelta = spanX;
    
    [self.mapView setRegion:[self.mapView regionThatFits:mapRegion] animated:YES];
    
    [self drawRoute:_coordinates];
    
    [self.mapView setShowsBuildings:YES];
    [self getHistory];
}
// Do any additional setup after loading the view.


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}




- (IBAction)doneButtonPressed:(id)sender {
    [self performSegueWithIdentifier:@"goBack" sender:self];
}


- (void)drawRoute:(NSMutableArray *)path
{
    NSInteger numberOfSteps = [path count];
    
    CLLocationCoordinate2D coordinates[numberOfSteps];
    for (NSInteger i = 0; i < numberOfSteps; i++) {
        CLLocation *location = [path objectAtIndex:i];
        CLLocationCoordinate2D coordinate = location.coordinate;
        
        coordinates[i] = coordinate;
    }
    
    MKPolyline *polyLine = [MKPolyline polylineWithCoordinates:coordinates count:[path count]];
    [self.mapView addOverlay:polyLine];
}

- (MKOverlayView *)mapView:(MKMapView *)mapView viewForOverlay:(id <MKOverlay>)overlay {
    MKPolylineView *polylineView = [[MKPolylineView alloc] initWithPolyline:overlay];
    polylineView.strokeColor = [UIColor redColor];
    polylineView.lineWidth = 10.0;
    polylineView.lineCap = kCGLineCapRound;
    polylineView.alpha = 1;
    
    return polylineView;
}

-(void)getHistory {
    matchStatsHead *result = [NetworkConnectionClass getMatchStats];
    
    if ((result->type[0] == 4) && (result->type[1] == 6)) {
        int winnerId = (int)result->array[0].winnerId;
        int userId = (int)result->array[0].userId;
        int dist = (int)result->array[0].distance;
        int ID = (int)result->array[0].userId;
        int speed = (int)result->array[0].averageSpeed;
        //_avgSpeedLabel.text = [NSString stringWithFormat:@"%d km/h", speed];
        _distanceLabel.text = [NSString stringWithFormat:@"%d km", _distance/1000];
        
        if (userId == winnerId) {
            _resultImageView.image = [UIImage imageNamed:@"star"];
            _resultLabel.text =@"You won!";
            
        } else if (winnerId == -1) {
            _resultImageView.image = [UIImage imageNamed:@"tie"];
            _resultLabel.text =@"It's a tie!";
        } else if (winnerId == 0) {
            _resultImageView.image = [UIImage imageNamed:@"tba"];
        } else {
            _resultImageView.image = [UIImage imageNamed:@"turd"];
            _resultLabel.text =@"You lost!";
        }
    }
}


/*
 #pragma mark - Navigation
 
 // In a storyboard-based application, you will often want to do a little preparation before navigation
 - (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
 {
 // Get the new view controller using [segue destinationViewController].
 // Pass the selected object to the new view controller.
 }
 */

@end
