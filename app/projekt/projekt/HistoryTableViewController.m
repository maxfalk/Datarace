//
//  HistoryTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 27/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "HistoryTableViewController.h"

@interface HistoryTableViewController ()

@end

@implementation HistoryTableViewController

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self addFooter];
     self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];
    
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


- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 3;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    return 70;
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    HistoryTableViewCell *cell = (HistoryTableViewCell*)[tableView dequeueReusableCellWithIdentifier:@"Cell"];
    
    if (cell == nil) {
        cell = [[HistoryTableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"Cell"] ;
    }
    
    if (indexPath.row == 0) {
        cell.username.text = @"Namn ett";
        cell.distance.text = [NSString stringWithFormat:@"%d km", 5];
        //cell.imageView.image = [UIImage imageNamed:@"turd"];
        [cell.btn setBackgroundImage:[UIImage imageNamed:@"turd"] forState:UIControlStateNormal];
    } else if (indexPath.row == 1) {
        cell.username.text = @"Namn två";
        cell.distance.text = [NSString stringWithFormat:@"%d km", 3];
        [cell.btn setBackgroundImage:[UIImage imageNamed:@"star"] forState:UIControlStateNormal];
    } else if (indexPath.row == 2) {
        cell.username.text = @"Namn tre";
        cell.distance.text = [NSString stringWithFormat:@"%d km", 8];
        [cell.btn setBackgroundImage:[UIImage imageNamed:@"tie"] forState:UIControlStateNormal];
    }
    return cell;
}

- (void)addFooter {
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}


@end
