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
@property (nonatomic,retain) NetworkConnectionClass *networkConnection;
@end


@implementation projektViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    _networkConnection = [[NetworkConnectionClass alloc] init];
    [_networkConnection initNetworkCommunication];
    //[_networkConnection setDelegate:self];
    
    
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
    
    _wheel.hidden = NO;
    [_wheel startAnimating];
    
    
    
    int *result = [_networkConnection sendLoginPackage:(_usernameField.text) password:(_passwordField.text)];
    
    if (result == 0) {
        [self performSegueWithIdentifier:@"login" sender:self];
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
    }
    
    //[(NetworkConnectionClass *)self initNetworkCommunication];
    [_loginButton setTitle:@"" forState:UIControlStateNormal];
    [_passwordField resignFirstResponder];
    
    //double delayInSeconds = 0.1;
    //dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    //dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
    //if (NSStreamEventErrorOccurred) {
    //[self performSegueWithIdentifier:@"login" sender:self];
    /*if
     NSLog(@"stream failed");
     UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error"
     message:@"Could not connect to server"
     delegate:self
     cancelButtonTitle:@"OK"
     otherButtonTitles:@"Retry",nil];
     [alert show];
     
     } else {
     */
    //[self performSegueWithIdentifier:@"login" sender:self];
    // }
    
    // });
    /*
     
     const char *user = [_usernameField.text UTF8String];
     const char *pass = [_passwordField.text UTF8String];
     uint32_t structInfo = 0;
     uint32_t myInt32AsABigEndianNumber2 = CFSwapInt32HostToBig(structInfo);
     
     uint32_t myInt32Value = 101;
     uint32_t myInt32AsABigEndianNumber = CFSwapInt32HostToBig(myInt32Value);
     
     mystruct packet;
     packet.length = myInt32AsABigEndianNumber;
     packet.info = myInt32AsABigEndianNumber2;
     memset(packet.username, 0, 50);
     strcpy(packet.username, user);
     memset(packet.password, 0, 50);
     strcpy(packet.password, pass);
     
     loginOutput message;
     
     
     //NSData *usernameData = [[NSData alloc] initWithData:
     //char buffer[2];
     //[inputStream read:((uint8_t *)&buffer) maxLength:sizeof(buffer)];
     
     NSInteger result;
     uint8_t buffer[20]; // BUFFER_LEN can be any positive integer
     //[inputStream read:buffer maxLength:sizeof(buffer)];
     if(buffer) {
     //login success
     } else {
     
     }
     */
    _wheel.hidden=YES;
    [_wheel stopAnimating];
    [_loginButton setTitle:@"Login" forState:UIControlStateNormal];
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


@end


