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

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.

    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
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
    
    _distanceLabel.text = [NSString stringWithFormat:@"%li km",(long)val];
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
