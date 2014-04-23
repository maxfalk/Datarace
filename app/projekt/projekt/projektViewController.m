//
//  projektViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define SYSTEM_VERSION_GREATER_THAN_OR_EQUAL_TO(v)  ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] != NSOrderedAscending)

#import "projektViewController.h"

typedef struct __attribute__ ((packed)) {
    uint32_t length;
    char info;
    char username [50];
    char password [50];
} mystruct;

@interface projektViewController ()
@property (weak, nonatomic) IBOutlet UITextField *usernameField;
@property (weak, nonatomic) IBOutlet UITextField *passwordField;
@property (weak, nonatomic) IBOutlet UIActivityIndicatorView *wheel;

@end

@implementation projektViewController 

- (void)viewDidLoad {
    NSLog(@"%lu", sizeof(mystruct));
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
    
    locationManager = [[CLLocationManager alloc] init];
    locationManager.delegate = self;
    [locationManager startUpdatingLocation];
    
    NSLog(@"latitude= %f longitude = %f",locationManager.location.coordinate.latitude, locationManager.location.coordinate.latitude);

    UIColor *babyBlue = [UIColor colorWithRed:0.4 green:0.6 blue:0.72 alpha:1];
    
    self.navigationController.navigationBar.barTintColor = babyBlue;
    
   // _passwordField = [[UITextField alloc] init];
    _passwordField.borderStyle = UITextBorderStyleRoundedRect;
    _passwordField.textColor = [UIColor blackColor];
   // _passwordField.font = [UIFont systemFontOfSize:12.0];
    [_passwordField setSecureTextEntry:YES];
    
    _usernameField.delegate = self;
    _passwordField.delegate = self;
    
    _wheel.hidden=YES;
    NSLog(@"%lu", (sizeof(mystruct)));
}

- (void)viewWillAppear:(BOOL)animated
{
    [self.navigationController setNavigationBarHidden:YES animated:animated];
    [super viewWillAppear:animated];
   }

- (void)viewWillDisappear:(BOOL)animated {
    [self.navigationController setNavigationBarHidden:NO animated:animated];
    [super viewWillDisappear:animated];
}

- (void)didReceiveMemoryWaring
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations{
    NSLog(@"lat:%f long:%f", manager.location.coordinate.latitude, manager.location.coordinate.latitude);
}


- (BOOL)textFieldShouldReturn:(UITextField *)theTextField {
    if (theTextField == _passwordField) {
        [theTextField resignFirstResponder];
    } else if (theTextField == _usernameField) {
        [_passwordField becomeFirstResponder];
    }
    return YES;
}

-(BOOL)textViewShouldEndEditing:(UITextView *)textView{
    [textView resignFirstResponder];
    return YES;
}

- (IBAction)loginButtonPressed:(id)sender {
   
    _wheel.hidden = NO;
    [_wheel startAnimating];
    [self initNetworkCommunication];
    [_loginButton setTitle:@"" forState:UIControlStateNormal];
    [_passwordField resignFirstResponder];
    double delayInSeconds = 0.1;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        if (true /* login success*/ ) {
         [self performSegueWithIdentifier:@"login" sender:self];
        } else {
            NSLog(@"Login failed");
        }
        _wheel.hidden=YES;
        [_wheel stopAnimating];
        [_loginButton setTitle:@"Logga in" forState:UIControlStateNormal];
    });
    
    const char *user = [_usernameField.text UTF8String];
    const char *pass = [_passwordField.text UTF8String];

    
    uint32_t myInt32Value = 101;
    uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
    
    mystruct packet;
    packet.length = myInt32AsABigEndianNumber;
    *packet.username = *user;
    *packet.username = *pass;
    
    //NSData *usernameData = [[NSData alloc] initWithData:
    
    [outputStream write:((const uint8_t *)&packet) maxLength:sizeof(mystruct)];
    //[outputStream write:mystruct maxLength:[usernameData length]];

}

-(void)initNetworkCommunication {
    CFReadStreamRef readStream;
    CFWriteStreamRef writeStream;
    CFStreamCreatePairWithSocketToHost(NULL, (CFStringRef)@"localhost", 8888, &readStream, &writeStream);
    inputStream = (__bridge NSInputStream *)readStream;
    outputStream = (__bridge NSOutputStream *)writeStream;
    [inputStream setDelegate:self];
    [outputStream setDelegate:self];
    [inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    
    [inputStream open];
    [outputStream open];
    
}

- (void)addBounceAnimationToView:(UIView *)view {
    CAKeyframeAnimation *bounceAnimation = [CAKeyframeAnimation animationWithKeyPath:@"transform.scale"];
    
    bounceAnimation.values = @[@(1), @(0.9), @(1.1), @(1)];
    
    bounceAnimation.duration = 0.6;
    NSMutableArray *timingFunctions = [[NSMutableArray alloc] initWithCapacity:bounceAnimation.values.count];
    
    for (NSUInteger i = 0; i < bounceAnimation.values.count; i++) {
        [timingFunctions addObject:[CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut]];
    }
    
    [bounceAnimation setTimingFunctions:timingFunctions.copy];
    bounceAnimation.removedOnCompletion = NO;
    
    [view.layer addAnimation:bounceAnimation forKey:@"bounce"];
}
/*
-(void) sendDataToServer:(CLLocation *)newLocation
{
    NSLog(@"Sending Data to Server");
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // I also want to send the battery level to the server. Get battery level
        [[UIDevice currentDevice] setBatteryMonitoringEnabled:YES];
        float batteryLevel = [[UIDevice currentDevice] batteryLevel];
        
        float lat = newLocation.coordinate.latitude;
        float lng = newLocation.coordinate.longitude;
        NSLog(@"Accuracy: %f", newLocation.horizontalAccuracy);
        NSString *userId = [[NSUserDefaults standardUserDefaults] stringForKey:@"userId"];
        
        // This is the data I am sending to the server
        // I am sending a userID that the server recognizes
        // I am sending the latitude and longitude of the user as well as their speed course and battery life
        // I am also sending the horizontal & vertical accuracy so I can see how accurate the gps location was
        NSString *post = [[NSString alloc] initWithFormat:@"login_id=%@&latitude=%f&longitude=%f&speed=%f&course=%f&battery_level=%f&horizontal_accuracy=%f&vertical_accuracy=%f",
                          userId,
                          lat,
                          lng,
                          [newLocation speed],
                          [newLocation course],
                          batteryLevel,
                          newLocation.horizontalAccuracy,
                          newLocation.verticalAccuracy];
        
        NSData *postData = [post dataUsingEncoding:NSASCIIStringEncoding allowLossyConversion:YES];
        
        NSString *postLength = [NSString stringWithFormat:@"%lu", (unsigned long)[postData length]];
        
        NSMutableURLRequest *request = [[NSMutableURLRequest alloc] init];
        NSString *urlstring = [NSString stringWithFormat:@"%@webservice/post_logins_location.php", kBaseURL];
        [request setURL:[NSURL URLWithString:urlstring]];
        [request setHTTPMethod:@"POST"];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
        [request setHTTPBody:postData];
        
        NSError *error;
        NSURLResponse *response;
        NSData *urlData = [NSURLConnection sendSynchronousRequest:request returningResponse:&response error:&error];
        
        if (!error) {
            jsonResults = [NSJSONSerialization JSONObjectWithData:urlData options:kNilOptions error:&error];
            NSLog(@"GPS Send results: %@", jsonResults);
        } else {
            NSLog(@"Error sending GPS data to server");
        }
    });
}
*/


@end


