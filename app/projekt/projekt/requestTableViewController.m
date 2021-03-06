//
//  requestTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 17/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define   DEGREES_TO_RADIANS(degrees)  ((3.14159265359 * (degrees-90))/ 180)
#define OFFSET ((3.14159265359 * (10))/ 180)
#define ARC4RANDOM_MAX      0x100000000

#import "requestTableViewController.h"

@interface requestTableViewController ()
@property (strong, nonatomic) NSMutableArray *requests;
@property (strong, nonatomic) NSMutableArray *myRequests;
@property (strong, nonatomic) NSMutableArray *myRequestsDistances;
@property (strong, nonatomic) NSMutableArray *myRequestsIDs;
@property (strong, nonatomic) NSMutableArray *distances;
@property (strong, nonatomic) NSMutableArray *requestIDs;

@end

@implementation requestTableViewController

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
    

            [self addFooter];
    
}


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    if (section==0) {
        return [_requests count];
    } else if (section==1){
        return 1;
        //[_myRequests count];
    } else
        return 0;
}


- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
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
    
    
        cell.usernameLabel.text = [_requests objectAtIndex:indexPath.row];

        cell.distanceLabel.text = [_distances objectAtIndex:indexPath.row];
            
            
       /*     UIButton *declineButton = [[UIButton alloc] initWithFrame:CGRectMake(260, 35, 34, 34)];
            //[closeBtn setImage:[UIImage imageNamed:@"close"] forState:UIControlStateNormal];
           [declineButton addTarget:self action:@selector(declineButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
            [declineButton setImage:[UIImage imageNamed:@"decline"] forState:UIControlStateNormal];
            [declineButton setTag:indexPath.row];
            
            UIButton *acceptButton = [[UIButton alloc] initWithFrame:CGRectMake(210, 35, 34, 34)];
            [acceptButton addTarget:self action:@selector(acceptButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
            [acceptButton setImage:[UIImage imageNamed:@"accept"] forState:UIControlStateNormal];
            
            [cell addSubview:declineButton];
            [cell addSubview:acceptButton];
        */
    
        //cell.selectionStyle = UITableViewCellSelectionStyleNone;;
     /*
    } else if (indexPath.section == 1) {
        if (_myRequests != nil) {
            cell.usernameLabel.text = [_myRequests objectAtIndex:indexPath.row];
            //[_myRequests objectAtIndex:indexPath.row];
            
            //cell.primaryLabelTwo.text = @"Distance";
            // cell.distanceLabel.text = [NSString stringWithFormat:@"%@", [_myRequestsDistances objectAtIndex:indexPath.row]];
            cell.distanceLabel.textAlignment = NSTextAlignmentCenter;
        } */
    
    
    NSLog(@"indexPath: %ld", (long)indexPath.row);
    return cell;

}

-(void)declineButtonPressed:(id)sender
{
    NSLog(@"declined!");
    NSIndexPath *indexPath = [NSIndexPath indexPathForRow:[sender tag] inSection:0];
    int reqID = (int) [[_requestIDs objectAtIndex:indexPath.row] integerValue];
    
    [NetworkConnectionClass cancelRequest:reqID];
    
    [_requests removeObjectAtIndex:indexPath.row];
    [_requestIDs removeObjectAtIndex:indexPath.row];
    [_distances removeObjectAtIndex:indexPath.row];
    NSLog(@"Declined with req id: %d", reqID);
    
    [self.tableView reloadData];
}

-(void)acceptButtonPressed:(id)sender
{
    NSIndexPath *indexPath = [NSIndexPath indexPathForRow:[sender tag] inSection:0];
    int reqID = (int)[[_requestIDs objectAtIndex:indexPath.row] integerValue];
    [NetworkConnectionClass acceptRequest:reqID];
    NSLog(@"Accepted with req id: %d", reqID);
    [self.tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath]
                          withRowAnimation:UITableViewRowAnimationFade];
    
    [self.tableView reloadData];
}

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex {
    
    
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
        /*
        lookUpResultGot->requestLookUp = nil;
        lookUpResultMade->requestLookUp = nil;
        lookUpResultGot = nil;
        lookUpResultMade = nil;
        
        free(lookUpResultGot->requestLookUp);
        free(lookUpResultMade->requestLookUp);
        free(lookUpResultGot);
        free(lookUpResultMade);
         */
        
        
    
}


- (void)addFooter {
    
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}

@end
