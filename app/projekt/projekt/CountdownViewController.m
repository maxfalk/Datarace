//
//  CountdownViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 08/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "CountdownViewController.h"

@interface CountdownViewController ()
@property (weak, nonatomic) IBOutlet UILabel *countdownLabel;
@property (strong, nonatomic) NSTimer *timer;
@property (nonatomic) NSInteger time;

@end

@implementation CountdownViewController

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
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    _countdownLabel.text = @"3";
    _time = 4;
    _timer = [[NSTimer alloc] init];
    [self countDownTimer:_timer];
}

-(void)countDown:(NSTimer *)timer {
    if (_time == 1) {
        _countdownLabel.text = @"GO!";
        _time--;
        
    } else if (_time == 0) {
        [timer invalidate];
        [self dismissViewControllerAnimated:YES completion:nil];
        _time--;
        
    } else {
        _countdownLabel.text = [NSString stringWithFormat:@"%li", (long)_time-1];
        --_time;
    }
}

-(void)countDownTimer:(NSTimer *)timer {
    [NSTimer scheduledTimerWithTimeInterval:1
                                     target:self
                                   selector:@selector(countDown:)
                                   userInfo:nil
                                    repeats:YES];
}



- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


@end
