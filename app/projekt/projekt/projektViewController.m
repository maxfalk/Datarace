//
//  projektViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define SYSTEM_VERSION_GREATER_THAN_OR_EQUAL_TO(v)  ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] != NSOrderedAscending)

#import "projektViewController.h"



@interface projektViewController ()
@property (weak, nonatomic) IBOutlet UITextField *usernameField;
@property (weak, nonatomic) IBOutlet UITextField *passwordField;
@property (weak, nonatomic) IBOutlet UIActivityIndicatorView *wheel;
@end


@implementation projektViewController

- (void)viewDidLoad {
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
    
    
    //[NetworkConnectionClass initNetworkCommunication];
    _wheel.hidden = NO;
    [_wheel startAnimating];
    
    
    /*
    int result = [NetworkConnectionClass sendLoginPackage:(_usernameField.text) password:(_passwordField.text)];
    
    if (result == 0) {
        [self performSegueWithIdentifier:@"login" sender:self.networkConnection];
    } else if (result == 1) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error"
                                                        message:@"Wrong username"
                                                       delegate:self
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        [alert show];
    } else if (result == 2) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error"
                                                        message:@"Wrong password"
                                                       delegate:self
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        [alert show];
    } else if (result == 3) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error"
                                                        message:@"Could not connect to server"
                                                       delegate:self
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
        [alert show];
    }
    
    //[(NetworkConnectionClass *)self initNetworkCommunication];
    [_loginButton setTitle:@"" forState:UIControlStateNormal];
    [_passwordField resignFirstResponder];
    
    double delayInSeconds = 0.3;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        
    });


    _wheel.hidden=YES;
    [_wheel stopAnimating];
    [_loginButton setTitle:@"Login" forState:UIControlStateNormal];
     
    
    */
     [self performSegueWithIdentifier:@"login" sender:self];
}


/*
 
 - (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)eventCode {
 NSLog(@"got an event");
 switch (eventCode) {
 case NSStreamEventHasSpaceAvailable:
 NSLog(@"None!");
 break;
 case NSStreamEventOpenCompleted:
 NSLog(@"Stream opened");
 break;
 case NSStreamEventHasBytesAvailable:
 NSLog(@"NSStreamEventHasBytesAvail");
 
 
 [inputStream close];
 [inputStream removeFromRunLoop:[NSRunLoop currentRunLoop]forMode:NSDefaultRunLoopMode];
 break;
 case NSStreamEventErrorOccurred:
 NSLog(@"CONNECTION ERROR: Connection to the host  failed!");
 break;
 case NSStreamEventEndEncountered:
 NSLog(@"Stream Closed");
 break;
 default:
 break;
 }
 }
 */

/*
 -(void)initNetworkCommunication {
 CFReadStreamRef readStream;
 CFWriteStreamRef writeStream;
 CFStreamCreatePairWithSocketToHost(NULL, (CFStringRef)@"83.253.5.227", 8888, &readStream, &writeStream);
 inputStream = (__bridge NSInputStream *)readStream;
 outputStream = (__bridge NSOutputStream *)writeStream;
 [inputStream setDelegate:self];
 [outputStream setDelegate:self];
 [inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
 [outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
 
 [inputStream open];
 [outputStream open];
 
 }
 */

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

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex {
    
    if (buttonIndex == 1) {
        [self loginButtonPressed:self];
    }
}

-(void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    if ([segue.identifier isEqualToString:@"signup"]) {
        self.networkConnection = (NetworkConnectionClass *)sender;
        SignupViewController *connection = (SignupViewController *) [segue destinationViewController];
        connection.networkConnection = self.networkConnection;
    }
}


@end


