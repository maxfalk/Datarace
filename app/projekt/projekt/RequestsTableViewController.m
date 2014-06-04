//
//  RequestsTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 27/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "RequestsTableViewController.h"

@interface RequestsTableViewController ()

@property (strong, nonatomic) NSMutableArray *requests;
@property (strong, nonatomic) NSMutableArray *myRequests;
@property (strong, nonatomic) NSMutableArray *myRequestsDistances;
@property (strong, nonatomic) NSMutableArray *myRequestsIDs;
@property (strong, nonatomic) NSMutableArray *distances;
@property (strong, nonatomic) NSMutableArray *requestIDs;


@end

@implementation RequestsTableViewController

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
    _requests = [[NSMutableArray alloc] init];
    _distances  = [[NSMutableArray alloc] init];
    _requestIDs = [[NSMutableArray alloc] init];
    _myRequests = [[NSMutableArray alloc] init];
    _myRequestsDistances= [[NSMutableArray alloc] init];
    _myRequestsIDs = [[NSMutableArray alloc] init];
    
    self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];
    
    @autoreleasepool {
        [self performSelectorInBackground:@selector(getRequests) withObject:nil];
    }
     _notification = [CWStatusBarNotification new];
    
    [self addFooter];
    
 }

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    if (([_requests count] + [_myRequests count]) == 0) {
        
            self.tableView.separatorColor = [UIColor clearColor];
            UIImageView *tempImageView = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"norequestsfound"]];
            self.tableView.backgroundView = tempImageView;
        return 0;
    } else {
    self.tableView.backgroundView = nil;
    if (section==0) {
        return [_requests count];
    } else if (section==1){
        return [_myRequests count];
    } else {
        return 0;
    }
    }
}




- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 2;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section {
    
    if(section == 0) {
        return @"Requests sent to you";
    } else {
        return @"Waiting for...";
    }
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    return 70;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    RequestTableViewCell *cell = (RequestTableViewCell *)[self.tableView dequeueReusableCellWithIdentifier:@"Cell"];
    
    if (cell == nil) {
        cell = [[RequestTableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"Cell"] ;
    }
    
    if (indexPath.section == 0) {
    cell.usernameLabel.text = [_requests objectAtIndex:indexPath.row];
    cell.distanceLabel.text = [NSString stringWithFormat:@"%@ km",[_distances objectAtIndex:indexPath.row]];
    cell.selectionStyle = UITableViewCellSelectionStyleNone;

    
    UIButton *declineButton = [[UIButton alloc] initWithFrame:CGRectMake(260, 18, 34, 34)];
    //[closeBtn setImage:[UIImage imageNamed:@"close"] forState:UIControlStateNormal];
    [declineButton addTarget:self action:@selector(declineButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [declineButton setImage:[UIImage imageNamed:@"decline"] forState:UIControlStateNormal];
    [declineButton setTag:indexPath.row];
    
    UIButton *acceptButton = [[UIButton alloc] initWithFrame:CGRectMake(210, 18, 34, 34)];
    [acceptButton addTarget:self action:@selector(acceptButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [acceptButton setImage:[UIImage imageNamed:@"accept"] forState:UIControlStateNormal];
    
    [cell addSubview:declineButton];
    [cell addSubview:acceptButton];
    } else {
        cell.usernameLabel.text = [_myRequests objectAtIndex:indexPath.row];
        cell.distanceLabel.text = [NSString stringWithFormat:@"%@ km",[_myRequestsDistances objectAtIndex:indexPath.row]];
        self.tableView.separatorColor = [UIColor lightGrayColor];
      //  cell.detailTextLabel.text = [NSString stringWithFormat:@"%@ km",[_myRequestsDistances objectAtIndex:indexPath.row]];
    }
    
    return cell;
    
}

-(void)declineButtonPressed:(id)sender
{
    UIColor *red = [UIColor colorWithRed:0.91 green:0.04 blue:0.09 alpha:1];
    NSIndexPath *indexPath = [NSIndexPath indexPathForRow:[sender tag] inSection:0];
    int reqID = (int) [[_requestIDs objectAtIndex:indexPath.row] integerValue];
    
    
    
    int distance = (int)[[_distances objectAtIndex:indexPath.row] integerValue];
    NSString *user = [NSString stringWithFormat:@"%@", [_requests objectAtIndex:indexPath.row]];
    NSString *string = [NSString stringWithFormat:@"Declined the %d km challenge from %@", distance, user];
    
    _notification.notificationLabelBackgroundColor = red;
    
    [self.notification displayNotificationWithMessage:string
                                          forDuration:2.0f];

    
    [NetworkConnectionClass cancelRequest:reqID];
    
    [_requests removeObjectAtIndex:indexPath.row];
    [_requestIDs removeObjectAtIndex:indexPath.row];
    [_distances removeObjectAtIndex:indexPath.row];
    
    [self.tableView reloadData];
}

-(void)acceptButtonPressed:(id)sender
{
    UIColor *green = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
    NSIndexPath *indexPath = [NSIndexPath indexPathForRow:[sender tag] inSection:0];
    int reqID = (int)[[_requestIDs objectAtIndex:indexPath.row] integerValue];
    [NetworkConnectionClass acceptRequest:reqID];
    
    int distance = (int)[[_distances objectAtIndex:indexPath.row] integerValue];
    NSString *user = [NSString stringWithFormat:@"%@", [_requests objectAtIndex:indexPath.row]];
    NSString *string = [NSString stringWithFormat:@"Accepted the %d km challenge from %@", distance, user];
    
    _notification.notificationLabelBackgroundColor = green;
    
    [self.notification displayNotificationWithMessage:string
                                          forDuration:2.0f];
    
    [_requests removeObjectAtIndex:indexPath.row];
    [_requestIDs removeObjectAtIndex:indexPath.row];
    [_distances removeObjectAtIndex:indexPath.row];
   
    
    [self.tableView reloadData];
}


-(void)getRequests {
    
    [NetworkConnectionClass sendRequest];
    
    requestLookUpResult *lookUpResultMade = [NetworkConnectionClass getRequests:2 type2:4];
    requestLookUpResult *lookUpResultGot = [NetworkConnectionClass getRequests:2 type2:5];
    
    _requests = [[NSMutableArray alloc] init];
    _distances  = [[NSMutableArray alloc] init];
    _requestIDs = [[NSMutableArray alloc] init];
    
    
    if (lookUpResultMade != nil) {
        int numOfPackesMade = lookUpResultMade->requestLookUpMeta.length/(sizeof(requestLookUp));
        
        for(int i = 0; i < numOfPackesMade; i++){
            
            if (lookUpResultMade->requestLookUp[i].state == 0) {
                NSString *usernameMade =[NSString stringWithFormat:@"%s",lookUpResultMade->requestLookUp[i].username];
                int distance = lookUpResultMade->requestLookUp[i].distance;
                int requestID = lookUpResultMade->requestLookUp[i].requestID;
                [_myRequests addObject:usernameMade];
                [_myRequestsDistances addObject:[NSNumber numberWithInt:distance]];
                [_myRequestsIDs addObject:[NSNumber numberWithInt:requestID]];
                
            }
        }
    }
    
    if (lookUpResultGot != nil) {
        int numOfPackesGot = lookUpResultGot->requestLookUpMeta.length/(sizeof(requestLookUp));
        
        for(int i = 0; i < numOfPackesGot; i++){
            if (lookUpResultGot->requestLookUp[i].state == 0) {
                NSString *usernameGot = [NSString stringWithFormat:@"%s",lookUpResultGot->requestLookUp[i].username];
                int distance = lookUpResultGot->requestLookUp[i].distance;
                int requestID = lookUpResultGot->requestLookUp[i].requestID;
                [_requests addObject:usernameGot];
                [_distances addObject:[NSNumber numberWithInt:distance]];
                [_requestIDs addObject:[NSNumber numberWithInt:requestID]];
            }
        }
    }
    
    
    [self.tableView reloadData];
    
}

- (void)tableView:(UITableView *)tableView willDisplayHeaderView:(UIView *)view forSection:(NSInteger)section {
    // Background color
    view.tintColor = [UIColor colorWithRed:0.96 green:0.96 blue:0.96 alpha:1];
    
    // Text Color
    UITableViewHeaderFooterView *header = (UITableViewHeaderFooterView *)view;
    [header.textLabel setTextColor:[UIColor blackColor]];
    
}



- (void)addFooter {
    
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}

@end
