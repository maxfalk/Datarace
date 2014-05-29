//
//  HistoryTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 27/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "HistoryTableViewController.h"

@interface HistoryTableViewController ()
@property (nonatomic, strong) NSMutableArray *usernames;
@property (nonatomic, strong) NSMutableArray *winnerID;
@property (nonatomic, strong) NSMutableArray *distance;
@property (nonatomic, strong) NSMutableArray *userID;


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
    _usernames = [[NSMutableArray alloc] init];
    _winnerID = [[NSMutableArray alloc] init];
    _distance = [[NSMutableArray alloc] init];
    _userID = [[NSMutableArray alloc] init];
    
    @autoreleasepool {
        [self performSelectorInBackground:@selector(getHistory) withObject:nil];
    }
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
    return [_usernames count];
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    return 70;
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    HistoryTableViewCell *cell = (HistoryTableViewCell*)[tableView dequeueReusableCellWithIdentifier:@"Cell"];
    
    if (cell == nil) {
        cell = [[HistoryTableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"Cell"] ;
    }
    
    if ([_usernames count] > 0) {
        
        int winnerID = (int)[[_winnerID objectAtIndex:indexPath.row] integerValue];
        cell.username.text = [_usernames objectAtIndex:indexPath.row];
        int distance = (int)[[_distance objectAtIndex:indexPath.row] integerValue];
        int ID = (int)[[_userID objectAtIndex:indexPath.row] integerValue];
        cell.distance.text = [NSString stringWithFormat:@"%d km", distance];
        cell.selectionStyle = UITableViewCellSelectionStyleNone;
        
        if (winnerID == 0) {
            [cell.btn setBackgroundImage:[UIImage imageNamed:@"tba"] forState:UIControlStateNormal];
        } else if (winnerID == -1) {
             [cell.btn setBackgroundImage:[UIImage imageNamed:@"tie"] forState:UIControlStateNormal];
        } else if (winnerID == ID) {
            [cell.btn setBackgroundImage:[UIImage imageNamed:@"star"] forState:UIControlStateNormal];
        } else if (winnerID != ID) {
            [cell.btn setBackgroundImage:[UIImage imageNamed:@"turd"] forState:UIControlStateNormal];
        }
        
    }
    return cell;
}
-(void)getHistory {
    [NetworkConnectionClass sendGetHistory];
    matchStatsHead *result = [NetworkConnectionClass getMatchStats];
    
    if ((result->type[0] == 3) && (result->type[1] == 3)) {
        for(int i = 0; i < result->length; i++) {
            NSString *username  = [NSString stringWithFormat:@"%s",result->array[i].username];
            int winnerId = (int)result->array[i].winnerId;
            int dist = (int)result->array[i].distance;
            int ID = (int)result->array[i].userId;
            [_usernames addObject:username];
            [_winnerID addObject:[NSNumber numberWithInt:winnerId]];
            [_distance addObject:[NSNumber numberWithInt:dist]];
            [_userID addObject:[NSNumber numberWithInt:ID]];
            
        }
        
        [self.tableView reloadData];
    }
}
- (void)addFooter {
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}


@end
