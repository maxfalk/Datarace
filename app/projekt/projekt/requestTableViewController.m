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

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    if (indexPath.section == 0) {
        return 100;
    } else
        return 44;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    if (section==0) {
        return [_requests count];
    } else if (section==1){
        return [_myRequests count];
    } else
        return 0;
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


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    CustomCell *cell = [[CustomCell alloc] initWithFrame:CGRectZero];
    cell.backgroundColor = [UIColor colorWithRed:0.96 green:0.96 blue:0.96 alpha:1];
    
    if (indexPath.section == 0) {
        if (_requests != nil)  {
            cell.primaryLabel.text = [_requests objectAtIndex:indexPath.row];
            
            
            cell.primaryLabelTwo.text = @"Distance";
            cell.distanceLabel.text = [NSString stringWithFormat:@"%@", [_distances objectAtIndex:indexPath.row]];
        }
        
        NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
        [formatter setNumberStyle:NSNumberFormatterDecimalStyle];
        [formatter setMaximumFractionDigits:2];
        [formatter setRoundingMode: NSNumberFormatterRoundDown];
        
        
        float winRatio = (float)(arc4random() % ((unsigned)RAND_MAX + 1)) / (float)((unsigned)RAND_MAX + 1);
        double winRatioToDegrees = winRatio * 360.0;
        UIBezierPath *path1 = [UIBezierPath bezierPathWithArcCenter:CGPointMake(60, 60)
                                                             radius:40
                                                         startAngle:DEGREES_TO_RADIANS(0)+OFFSET
                                                           endAngle:DEGREES_TO_RADIANS(winRatioToDegrees)-OFFSET
                                                          clockwise:YES];
        UIBezierPath *path2 = [UIBezierPath bezierPathWithArcCenter:CGPointMake(60, 60)
                                                             radius:40
                                                         startAngle:DEGREES_TO_RADIANS(winRatioToDegrees)+OFFSET
                                                           endAngle:DEGREES_TO_RADIANS(360)-OFFSET
                                                          clockwise:YES];
        
        UIColor *green = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
        UIGraphicsBeginImageContext(CGSizeMake(120, 120));
        [[UIColor blackColor] setStroke];
        path1.lineCapStyle = kCGLineCapRound;
        path1.lineWidth = 11.0f;
        [green setStroke];
        [path1 stroke];
        
        UIColor *red = [UIColor colorWithRed:0.91 green:0.04 blue:0.09 alpha:1];
        path2.lineWidth = 11.0f;
        path2.lineCapStyle = kCGLineCapRound;
        [red setStroke];
        [path2 stroke];
        
        
        
        
        UIImageView *drawpad = [[UIImageView alloc] initWithFrame:CGRectMake(20,30,60,60)];
        drawpad.image = UIGraphicsGetImageFromCurrentImageContext();
        
        [self drawRect:CGRectMake(0, 0, 100, 100)];
        
        UIGraphicsEndImageContext();
        [cell addSubview:drawpad];
        
        if (indexPath.section == 0) {
            UIButton *declineButton = [[UIButton alloc] initWithFrame:CGRectMake(260, 35, 34, 34)];
            //[closeBtn setImage:[UIImage imageNamed:@"close"] forState:UIControlStateNormal];
            [declineButton addTarget:self action:@selector(declineButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
            [declineButton setImage:[UIImage imageNamed:@"decline"] forState:UIControlStateNormal];
            [declineButton setTag:indexPath.row];
            
            UIButton *acceptButton = [[UIButton alloc] initWithFrame:CGRectMake(210, 35, 34, 34)];
            [acceptButton addTarget:self action:@selector(acceptButtonPressed:) forControlEvents:UIControlEventTouchUpInside];
            [acceptButton setImage:[UIImage imageNamed:@"accept"] forState:UIControlStateNormal];
            
            [cell addSubview:declineButton];
            [cell addSubview:acceptButton];
            
        }
        
        cell.selectionStyle = UITableViewCellSelectionStyleNone;;
        
    } else if (indexPath.section == 1) {
        if (_myRequests != nil) {
            cell.primaryLabel.text = [_myRequests objectAtIndex:indexPath.row];
            //[_myRequests objectAtIndex:indexPath.row];
            
            
            //cell.primaryLabelTwo.text = @"Distance";
            // cell.distanceLabel.text = [NSString stringWithFormat:@"%@", [_myRequestsDistances objectAtIndex:indexPath.row]];
            cell.distanceLabel.textAlignment = NSTextAlignmentCenter;
        }
    }
    
    return cell;
}

-(void)declineButtonPressed:(id)sender {
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

-(void)acceptButtonPressed:(id)sender {
    NSIndexPath *indexPath = [NSIndexPath indexPathForRow:[sender tag] inSection:0];
    int reqID = (int)[[_requestIDs objectAtIndex:indexPath.row] integerValue];
    [NetworkConnectionClass acceptRequest:reqID];
    NSLog(@"Accepted with req id: %d", reqID);
    
    [self.tableView reloadData];
    
}

- (void)drawRect:(CGRect)rect
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetStrokeColorWithColor(context, [[UIColor blueColor] CGColor]);
    
    UIBezierPath *blueHalf = [UIBezierPath bezierPath];
    [blueHalf addArcWithCenter:CGPointMake(100, 100) radius:90.0 startAngle:-M_PI_2 endAngle:M_PI_2 clockwise:YES];
    [blueHalf setLineWidth:4.0];
    [blueHalf stroke];
    
    CGContextSetStrokeColorWithColor(context, [[UIColor redColor] CGColor]);
    
    UIBezierPath *redHalf = [UIBezierPath bezierPath];
    [redHalf addArcWithCenter:CGPointMake(100.0, 100.0) radius:90.0 startAngle:M_PI_2 endAngle:3.0 * M_PI_2 clockwise:YES];
    [redHalf setLineWidth:4.0];
    [redHalf stroke];
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
