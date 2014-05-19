//
//  ChallengeViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 22/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "ChallengeViewController.h"

typedef struct __attribute__ ((packed)) {
    char info;
    char username [50];
    char password [50];
} mystruct;


@interface ChallengeViewController ()
@property (weak, nonatomic) IBOutlet UISlider *distanceSlider;

@end

@implementation ChallengeViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (IBAction)sendChallengeRequest:(id)sender {
    //requestLookUpResult *lookUpResultMade = [NetworkConnectionClass getRequests:2 type2:4];

    [NetworkConnectionClass makeRequest:40 distance:_distanceSlider.value];
}

- (void)viewDidLoad {
    [super viewDidLoad];
    _challenger.text = _challengerUsername;
    _updateDistanceSlider.maximumTrackTintColor = [UIColor colorWithRed:0.13 green:0.66 blue:0.82 alpha:1];
    _updateDistanceSlider.thumbTintColor = [UIColor colorWithRed:0.4 green:0.6 blue:0.72 alpha:1];
    _updateDistanceSlider.maximumValue = 5;
    
    _distanceSlider.maximumTrackTintColor = [UIColor colorWithRed:0.13 green:0.66 blue:0.82 alpha:1];
    _distanceSlider.thumbTintColor = [UIColor colorWithRed:0.4 green:0.6 blue:0.72 alpha:1];
    
}


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (IBAction)sliderChanged:(id)sender{
    UISlider *slider = (UISlider *)sender;
    NSInteger val = lround(slider.value);
    //[_distanceButton setTitle:[NSString stringWithFormat:@"%li km",(long)val] forState:UIControlStateNormal];
    
    _updateDistanceSlider.maximumValue = val-1;
    _distanceLabel.text = [NSString stringWithFormat:@"%li km",(long)val];
}

- (IBAction)updateDistanceSliderChanged:(id)sender{
    UISlider *slider = (UISlider *)sender;
    NSInteger val = lround(slider.value);
    //[_distanceButton setTitle:[NSString stringWithFormat:@"%li km",(long)val] forState:UIControlStateNormal];
    if (val == 0) {
        _updateDistanceLabel.text = @"All the time";
    } else {
    _updateDistanceLabel.text = [NSString stringWithFormat:@"%li km",(long)val];
    }
}




@end
