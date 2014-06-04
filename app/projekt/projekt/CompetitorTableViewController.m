//
//  CompetitorTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 28/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "CompetitorTableViewController.h"

@interface CompetitorTableViewController ()

@property (strong, nonatomic) NSMutableArray *competitors;
@property (strong, nonatomic) NSMutableArray *competitorsReady;
@property (strong, nonatomic) NSMutableArray *competitorsPending;
@property (nonatomic, strong) UIColor *green;
@property (nonatomic, strong) UIColor *red;
@property (strong, nonatomic) NSMutableArray *requests;
@property (strong, nonatomic) NSMutableArray *distances;
@property (strong, nonatomic) NSMutableArray *requestIDs;

//@property (nonatomic) UIButton *acceptButton;


@end

@implementation CompetitorTableViewController

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
    _competitorsPending = [[NSMutableArray alloc] init];
    _competitorsReady = [[NSMutableArray alloc] init];
    _requests = [[NSMutableArray alloc] init];
    _distances  = [[NSMutableArray alloc] init];
    _requestIDs = [[NSMutableArray alloc] init];
    self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];
    
    
    _green = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
    _red = [UIColor colorWithRed:0.91 green:0.04 blue:0.09 alpha:1];
    
}

-(void)viewDidAppear:(BOOL)animated {
    dispatch_async( dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Add code here to do background processing
        
        [_competitorsReady removeAllObjects];
        [_distances removeAllObjects];
        [_requestIDs removeAllObjects];
        
        [NetworkConnectionClass sendRequest];
        requestLookUpResult *lookUpResultMade = [NetworkConnectionClass getRequests:2 type2:4];
        requestLookUpResult *lookUpResultGot = [NetworkConnectionClass getRequests:2 type2:5];
        int numOfPackesMade = lookUpResultMade->requestLookUpMeta.length/(sizeof(requestLookUp));
        int numOfPackesGot = lookUpResultGot->requestLookUpMeta.length/(sizeof(requestLookUp));
        
        if (lookUpResultMade != nil) {
            for(int i = 0; i < numOfPackesMade; i++){
                if (lookUpResultMade->requestLookUp[i].state == 1) { //ready
                    NSString *usernameMade =[NSString stringWithFormat:@"%s",lookUpResultMade->requestLookUp[i].username];
                    int distance = lookUpResultMade->requestLookUp[i].distance;
                    int requestID = lookUpResultMade->requestLookUp[i].requestID;
                    [_competitorsReady addObject:usernameMade];
                    [_distances addObject:[NSNumber numberWithInt:distance]];
                    [_requestIDs addObject:[NSNumber numberWithInt:requestID]];
                }
            }
        }
        
        if (lookUpResultGot != nil) {
            for(int i = 0; i < numOfPackesGot; i++){
                if (lookUpResultGot->requestLookUp[i].state == 1) { //ready
                    NSString *usernameGot = [NSString stringWithFormat:@"%s",lookUpResultGot->requestLookUp[i].username];
                    int distance = lookUpResultGot->requestLookUp[i].distance;
                    int requestID = lookUpResultGot->requestLookUp[i].requestID;
                    [_competitorsReady addObject:usernameGot];
                    [_distances addObject:[NSNumber numberWithInt:distance]];
                    [_requestIDs addObject:[NSNumber numberWithInt:requestID]];
                    
                }
            }
        }
        
        
        free(lookUpResultGot->requestLookUp);
        free(lookUpResultMade->requestLookUp);
        free(lookUpResultGot);
        free(lookUpResultMade);
        numOfPackesGot = 0;
        numOfPackesMade = 0;
        
        
        
        dispatch_async( dispatch_get_main_queue(), ^{
            // Add code here to update the UI/send notifications based on the
            // results of the background processing
            [self addFooter];
             [self.tableView reloadData];
            
        });
    });
    
  

}
- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    return 60;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section {
    
    if(section == 0) {
        return @"READY";
    } else {
        return @"Available competitors";
    }
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {

    int count = (int)[_competitorsReady count]+[_competitorsPending count];
    
    if (count == 0) {
        self.tableView.separatorColor = [UIColor clearColor];
        UIImageView *tempImageView = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"nochallengesfound"]];
        self.tableView.backgroundView = tempImageView;
        return 0;
        
    } else
        self.tableView.backgroundView = nil;
        return count;
}



- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    CustomCell *cell = [[CustomCell alloc] initWithFrame:CGRectZero];
    cell.backgroundColor = [UIColor colorWithRed:0.96 green:0.96 blue:0.96 alpha:1];
    
    if (indexPath.section == 0) {
        cell.competitorLabel.text = [_competitorsReady objectAtIndex:indexPath.row];
        
        if (true) { //ready
            cell.statusLabel.text = @"READY";
            cell.statusLabel.textColor = _green;
        } else { //pending
            cell.statusLabel.text = @"Pending";
            cell.statusLabel.textColor = _red;
        }
    } else {
        cell.competitorLabel.text = [_competitors objectAtIndex:indexPath.row];
    }
    
    
    // cell.primaryLabelTwo.text = @"Distance";
    //  cell.distanceLabel.text = @"5 km";
    
    NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
    [formatter setNumberStyle:NSNumberFormatterDecimalStyle];
    [formatter setMaximumFractionDigits:2];
    [formatter setRoundingMode: NSNumberFormatterRoundDown];
    
    
    
    // UIButton *acceptButton = [[UIButton alloc] initWithFrame:CGRectMake(260, 13, 34, 34)];
    // [acceptButton addTarget:self action:@selector(acceptButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
    //[acceptButton setImage:[UIImage imageNamed:@"accept"] forState:UIControlStateNormal];
    
    
    cell.selectionStyle = UITableViewCellSelectionStyleBlue;
    //[cell addSubview:acceptButton];
    // acceptButton.hidden=YES;
    //cell.contentView = acceptButton;
    //acceptButton.hidden=YES;
    //[cell addSubview:acceptButton];
    //[cell.acceptButton setHidden:YES]:
    
    return cell;
}


- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    
    CustomCell *cell = (CustomCell *)[tableView cellForRowAtIndexPath:indexPath];
    //[tableView deselectRowAtIndexPath:indexPath animated:YES];
    
    if (indexPath.section == 0) {
        if ([cell.statusLabel.text isEqualToString:@"READY"]) {
            NSLog(@"Pressed on a READY cell");
            [self performSegueWithIdentifier:@"startChallenge" sender:indexPath];
            
            
        } else if ([cell.statusLabel.text isEqualToString:@"Pending"]) {
            NSLog(@"Pressed on a Pending cell");
        }
        
    } else if (indexPath.section == 1) {
        [self performSegueWithIdentifier:@"challengeSettings" sender:indexPath];
        
        
    }
}


-(void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {

    if ([segue.identifier isEqualToString:@"challengeSettings"]) {
        CustomCell *cell = (CustomCell *)[self.tableView cellForRowAtIndexPath:sender];
        NSString *string = cell.competitorLabel.text;
        ChallengeViewController *class = (ChallengeViewController *) [segue destinationViewController];
        class.challengerUsername = string;
    } else if ([segue.identifier isEqualToString:@"startChallenge"]) {
        NSIndexPath *indexPath = sender;
        RaceViewController *class = (RaceViewController *) [segue destinationViewController];
        class.reqID = (int)[[_requestIDs objectAtIndex:indexPath.row] integerValue];
        class.distance = (int)[[_distances objectAtIndex:indexPath.row] integerValue]*1000;
    } else if ([segue.identifier isEqualToString:@"search"]) {
        NSLog(@"Search ready to segue");
    }
}


- (void)addFooter {
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}

- (IBAction)searchButtonPressed:(id)sender {
    [self performSegueWithIdentifier:@"search" sender:self];
}

@end
