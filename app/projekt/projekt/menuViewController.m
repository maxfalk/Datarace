//
//  menuViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define   DEGREES_TO_RADIANS(degrees)  ((3.14159265359 * (degrees-90))/ 180)
#define OFFSET ((3.14159265359 * (10))/ 180)
#import "menuViewController.h"

@interface menuViewController ()
@property (weak, nonatomic) IBOutlet UILabel *user;
@property (weak, nonatomic) IBOutlet UIButton *requestLabel;
@property (weak, nonatomic) IBOutlet UILabel *averageDistanceLabel;
@property (weak, nonatomic) IBOutlet UILabel *averageSpeedLabel;
@property (nonatomic) double winRatio;
@property (weak, nonatomic) IBOutlet UILabel *lossesLabel;
@property (weak, nonatomic) IBOutlet UILabel *winsLabel;
@property (weak, nonatomic) IBOutlet UILabel *noStatsLabel;
@end

@implementation menuViewController
@synthesize drawpad;


- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    
    self.navigationItem.hidesBackButton = YES;
    

    //fetch username from server
    //_user.text = @"Joel Sandberg";
    
            //fetch win ratio from server
            _winRatio = 0.33;
    
    [self updateHomeStats];
        
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (UIBezierPath *)createArcPath
{
    UIBezierPath *aPath = [UIBezierPath bezierPathWithArcCenter:CGPointMake(150, 150)
                                                         radius:75
                                                     startAngle:0
                                                       endAngle:DEGREES_TO_RADIANS(135)
                                                      clockwise:YES];
    return aPath;
}

- (void)drawRect:(CGRect)rect
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetStrokeColorWithColor(context, [[UIColor blueColor] CGColor]);
    
    UIBezierPath *blueHalf = [UIBezierPath bezierPath];
    [blueHalf addArcWithCenter:CGPointMake(100, 100) radius:90.0 startAngle:-M_PI_2 endAngle:M_PI_2 clockwise:YES];
    [blueHalf setLineWidth:4.0];
    [blueHalf stroke];
    
    CGContextSetStrokeColorWithColor(context, [[UIColor redColor] CGColor]);
    
    UIBezierPath *redHalf = [UIBezierPath bezierPath];
    [redHalf addArcWithCenter:CGPointMake(100.0, 100.0) radius:90.0 startAngle:M_PI_2 endAngle:3.0 * M_PI_2 clockwise:YES];
    [redHalf setLineWidth:4.0];
    [redHalf stroke];
}


- (void)drawCircle
{
    CGPoint center = CGPointMake(100, 100);
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextBeginPath(ctx);
    
    //6 CGContextSetLineWidth(ctx, 5);
    CGContextAddArc(ctx, center.x, center.y, 100.0, 0, 2*M_PI, 0);
    CGContextStrokePath(ctx);
}

- (IBAction)signOutButtonPressed:(id)sender {
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Sign out"
                                                    message:@"Do you want to sign out?"
                                                   delegate:self
                                          cancelButtonTitle:@"No"
                                          otherButtonTitles:@"Yes",nil];
    [alert show];
    
}

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex {
    
    if (buttonIndex == 1) {
        [NetworkConnectionClass signOut];
        [self.navigationController popToRootViewControllerAnimated:YES];
        
    }
}

-(void)updateHomeStats {
    homeStats *result = (homeStats *)[NetworkConnectionClass getHomeStats];
    
    //username
    NSString *string = [NSString stringWithFormat:@"%s", result->username];
    _user.text = string;
 
    //requests
    int requestAC = (int)[NetworkConnectionClass getNumberOfPendingRequests];
    if (requestAC == 0) {
        _requestLabel.hidden=YES;
    } else {
    [_requestLabel setTitle:[NSString stringWithFormat:@"%i", requestAC] forState:UIControlStateNormal];
    }
    
    //average speed
    double speedBC = result->averageSpeed;
    _averageSpeedLabel.text = [NSString stringWithFormat:@"%.1f km", speedBC];
    
    //average distance
    double distanceBC = result->averageDistance;
    _averageDistanceLabel.text = [NSString stringWithFormat:@"%0.1f km/h", distanceBC];
    
    //wins
    double winsAC = result->wins;
    
    //matches
    double matchesAC = result->matches;
    
    //win ratio
    _winRatio = winsAC/matchesAC;
    
    if (matchesAC <= 0) {
        _noStatsLabel.text = @"No stats yet";
        _winsLabel.hidden=YES;
        _lossesLabel.hidden=YES;
    } else {
        _noStatsLabel.hidden=YES;
    double winRatioToDegrees = (_winRatio * 360);
    
    UIBezierPath *path1 = [UIBezierPath bezierPathWithArcCenter:CGPointMake(60, 60)
                                                         radius:50
                                                     startAngle:DEGREES_TO_RADIANS(0)+OFFSET
                                                       endAngle:DEGREES_TO_RADIANS(winRatioToDegrees)-OFFSET
                                                      clockwise:YES];
    
    UIBezierPath *path2 = [UIBezierPath bezierPathWithArcCenter:CGPointMake(60, 60)
                                                         radius:50
                                                     startAngle:DEGREES_TO_RADIANS(winRatioToDegrees)+OFFSET
                                                       endAngle:DEGREES_TO_RADIANS(360)-OFFSET
                                                      clockwise:YES];
    
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
    
    [self drawRect:CGRectMake(0, 0, 100, 100)];
    
    UIGraphicsEndImageContext();
    
    }
    
    /*CALayer *layer = [self.drawpad.layer sublayers][0];
	layer.transform = CATransform3DMakeRotation(10, 0, 1, 0);
     */
}


- (IBAction)unwindToAd:(UIStoryboardSegue *)unwindSegue
{
    
    UIViewController* sourceViewController = unwindSegue.sourceViewController;
    
    if ([sourceViewController isKindOfClass:[FinishlineViewController class]])
    {
        NSLog(@"Coming from race!");
    }
}


@end
