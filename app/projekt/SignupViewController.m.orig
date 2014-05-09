//
//  SignupViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 25/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "SignupViewController.h"

@interface SignupViewController ()

@property (weak, nonatomic) IBOutlet UITextField *usernameField;
@property (weak, nonatomic) IBOutlet UITextField *emailField;
@property (weak, nonatomic) IBOutlet UITextField *passwordField;
@property (weak, nonatomic) IBOutlet UITextField *passwordRepeatField;
@property (weak, nonatomic) IBOutlet UIImageView *usernameCheck;
@property (weak, nonatomic) IBOutlet UIImageView *emailCheck;
@property (weak, nonatomic) IBOutlet UIImageView *passwordCheck;
@property (weak, nonatomic) IBOutlet UIImageView *passwordRepeatCheck;
@property (strong, nonatomic) UIScrollView *scrollView;
@property (strong, nonatomic) IBOutlet UIButton *signupButton;
//@property (strong) NetworkConnectionClass *signUp;

@end

@implementation SignupViewController
//@synthesize networkConnection = _networkConnection;


- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    
    //_networkConnection = [[projektViewController alloc] init];
    //[_networkConnection initNetworkCommunication];
    //_signupButton.backgroundColor = [UIColor colorWithRed:0 green:205/255 blue:205/255 alpha:1];
    _scrollView = [[UIScrollView  alloc] initWithFrame:CGRectMake(0, 50, self.view.frame.size.width, 300)];
    _scrollView.scrollEnabled=YES;
    _scrollView.showsVerticalScrollIndicator = YES;
    _scrollView.pagingEnabled = YES;
    // Do any additional setup after loading the view.
    _usernameField.delegate = self;
    _emailField.delegate = self;
    _passwordField.delegate = self;
    _passwordRepeatField.delegate = self;
    
    CGRect frame;
    frame.origin.x = _scrollView.frame.size.width * self.view.bounds.size.width;
    frame.origin.y = 0;
    frame.size = _scrollView.frame.size;
    
    // [self.view addSubview:_scrollView];
    
    UIView *subview = [[UIView alloc] initWithFrame:frame];
    [subview addSubview:self.view];
    [_scrollView addSubview:subview];
    
    _scrollView.contentSize = CGSizeMake(_scrollView.frame.size.width, _scrollView.frame.size.height);
    
    /*_usernameCheck.backgroundColor = [UIColor clearColor];
     _emailCheck.backgroundColor = [UIColor clearColor];
     _passwordCheck.backgroundColor = [UIColor clearColor];
     _passwordRepeatCheck.backgroundColor = [UIColor clearColor];
     */
    
    _usernameCheck.image = [UIImage imageNamed:@"arrow"];
    _emailCheck.image = [UIImage imageNamed:@"arrow"];
    _passwordCheck.image = [UIImage imageNamed:@"arrow"];
    _passwordRepeatCheck.image = [UIImage imageNamed:@"arrow"];
    
}


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}



- (BOOL)textFieldShouldReturn:(UITextField *)theTextField {
    if (theTextField == _passwordRepeatField) {
        
        if ([_passwordField.text isEqual:_passwordRepeatField.text] && ![_passwordField.text isEqual:@""]) {
            _passwordRepeatCheck.image = [UIImage imageNamed:@"accept"];
            UIColor *orange = [UIColor colorWithRed:0.91 green:0.4 blue:0.36 alpha:1];
            [self.signupButton setBackgroundColor:orange];
        } else {
            _passwordRepeatCheck.image = [UIImage imageNamed:@"decline"];
        }
        [theTextField resignFirstResponder];
    } else if (theTextField == _usernameField) {
        [_emailField becomeFirstResponder];
        if ([_usernameField.text isEqual:@""]) {
            _usernameCheck.image = [UIImage imageNamed:@"decline"];
        } else {
            _usernameCheck.image = [UIImage imageNamed:@"accept"];
        }
    } else if (theTextField == _emailField) {
        [_passwordField becomeFirstResponder];
        if ([_emailField.text isEqual:@""]) {
            _emailCheck.image = [UIImage imageNamed:@"decline"];
        } else {
            _emailCheck.image = [UIImage imageNamed:@"accept"];
        }
        
    } else if (theTextField == _passwordField) {
        [_passwordRepeatField becomeFirstResponder];
        
        if ([_passwordField.text isEqual:@""]) {
            _passwordCheck.image = [UIImage imageNamed:@"decline"];
        } else {
            _passwordCheck.image = [UIImage imageNamed:@"accept"];
        }
    }
    return YES;
}

-(BOOL)textViewShouldEndEditing:(UITextView *)textView{
    [textView resignFirstResponder];
    return YES;
}

- (void)textFieldDidEndEditing:(UITextField *)theTextField{
    if (theTextField == _passwordRepeatField) {
        
        if ([_passwordField.text isEqual:_passwordRepeatField.text] && ![_passwordField.text isEqual:@""]) {
            _passwordRepeatCheck.image = [UIImage imageNamed:@"accept"];
            UIColor *orange = [UIColor colorWithRed:0.91 green:0.4 blue:0.36 alpha:1];
            [self.signupButton setBackgroundColor:orange];
        } else {
            _passwordRepeatCheck.image = [UIImage imageNamed:@"decline"];
        }
        
    } else if (theTextField == _usernameField) {
        if ([_usernameField.text isEqual:@""]) {
            _usernameCheck.image = [UIImage imageNamed:@"decline"];
        } else {
            _usernameCheck.image = [UIImage imageNamed:@"accept"];
        }
    } else if (theTextField == _emailField) {
        
        if ([_emailField.text isEqual:@""]) {
            _emailCheck.image = [UIImage imageNamed:@"decline"];
        } else {
            _emailCheck.image = [UIImage imageNamed:@"accept"];
        }
        
    } else if (theTextField == _passwordField) {
        
        if ([_passwordField.text isEqual:@""]) {
            _passwordCheck.image = [UIImage imageNamed:@"decline"];
        } else {
            _passwordCheck.image = [UIImage imageNamed:@"accept"];
        }
    }
    
}

- (IBAction)signupButtonPressed:(id)sender {
    [NetworkConnectionClass initNetworkCommunication];
    
    dispatch_async( dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        
        int result = [NetworkConnectionClass signupUser:_usernameField.text password:_passwordField.text email:_emailField.text];
        
        
        dispatch_async( dispatch_get_main_queue(), ^{
            if (result == 0) {
                [self performSegueWithIdentifier:@"signupSuccess" sender:self];
            } else if (result == 1) {
                UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Error"
                                                                message:@"Signup unsuccessful"
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
        });
        
    });
}




- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex {
    
    
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
