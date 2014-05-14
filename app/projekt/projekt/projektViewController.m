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
@property (strong, nonatomic) NSTimer *time;
@property CLLocationCoordinate2D myCoordinate;

@end


@implementation projektViewController
//@synthesize locationManager;

- (void)viewDidLoad {
    [super viewDidLoad];
    
    
	// Do any additional setup after loading the view, typically from a nib.
    
    //self.locationManager = [[CLLocationManager alloc] init];
    //self.locationManager.delegate = self;
   // [self.locationManager startUpdatingLocation];

    
    
    //self.time = [NSTimer scheduledTimerWithTimeInterval:10 target: locationManager selector:@selector(startUpdatingLocation) userInfo:nil repeats:YES];
    
    
    //[NetworkConnectionClass sendUpdatedCoordinates];
    
    //
    
    [super viewDidLoad];
    
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
    
    //[self updateLocation];
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
    dispatch_async( dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Add code here to do background processing
       [NetworkConnectionClass initNetworkCommunication];
        int result = [NetworkConnectionClass sendLoginPackage:(_usernameField.text) password:(_passwordField.text)];
        //int result = 0;


        dispatch_async( dispatch_get_main_queue(), ^{
        // Add code here to update the UI/send notifications based on the
        // results of the background processing
        
        if (result == 0) {
            _usernameField.text = @"";
            _passwordField.text = @"";
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
        } else if (result == 4) {
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error"
                                                            message:@"Already signed in on another device"
                                                           delegate:self
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles:nil];
            [alert show];
        }
        
            _wheel.hidden=YES;
            [_wheel stopAnimating];
            [_loginButton setTitle:@"Login" forState:UIControlStateNormal];
        });
    });
    
    
    
    [_loginButton setTitle:@"" forState:UIControlStateNormal];
    [_passwordField resignFirstResponder];
    
    double delayInSeconds = 0.3;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        
    });
    
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
   // dispatch_async( dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        if ([segue.identifier isEqualToString:@"signup"]) {
            self.networkConnection = (NetworkConnectionClass *)sender;
            SignupViewController *connection = (SignupViewController *) [segue destinationViewController];
            connection.networkConnection = self.networkConnection;
        }
        
      //  NSLog(@"Finished work in background");
        //dispatch_async( dispatch_get_main_queue(), ^{
          //  NSLog(@"Back on main thread");
        //});
                       
    //});
}

    
        @end
                       
                       
