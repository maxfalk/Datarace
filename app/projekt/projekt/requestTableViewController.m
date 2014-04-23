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
    
    //fetch requests from server
    _requests = [[NSMutableArray alloc] initWithArray:@[@"Babak Toghiani-Rizi", @"Marina Jaksic", @"Max Falk Nilsson", @"Max Reeves"]];
    
    self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];
    [self addFooter];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source
/*
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
#warning Potentially incomplete method implementation.
    // Return the number of sections.
    return 0;
}
 */


- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    
        return 100;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{

    // Return the number of rows in the section.
    return [_requests count];
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    CustomCell *cell = [[CustomCell alloc] initWithFrame:CGRectZero];
    
    cell.primaryLabel.text = [_requests objectAtIndex:indexPath.row];
    cell.backgroundColor = [UIColor colorWithRed:0.96 green:0.96 blue:0.96 alpha:1];
    
    cell.primaryLabelTwo.text = @"Distance";
    cell.distanceLabel.text = @"5 km";
    
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
    
    cell.selectionStyle = UITableViewCellSelectionStyleNone;;


    return cell;
}

-(void)declineButtonPressed:(id)sender {
    NSLog(@"declined!");

    /*
     UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Huh?!"
                                                   message:@"Do you really wanna chicken out on this one?"
                                                  delegate:self
                                         cancelButtonTitle:@"Yes..."
                                         otherButtonTitles:@"NO!!", nil];
    [alert show];
     */
    
    NSIndexPath *indexPath = [NSIndexPath indexPathForRow:[sender tag] inSection:0];
    [_requests removeObjectAtIndex:indexPath.row];
    [self.tableView reloadData];
}

-(void)acceptButtonPressed:(id)sender {
    NSLog(@"accepted!");
    
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"WHOOP!"
                                                    message:@"Are you ready?"
                                                   delegate:self
                                          cancelButtonTitle:@"YES!"
                                          otherButtonTitles:@"No... not yet...", nil];
    [alert show];
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

/*
// Override to support conditional editing of the table view.
- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Return NO if you do not want the specified item to be editable.
    return YES;
}
*/

/*
// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
{
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        // Delete the row from the data source
        [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
    } else if (editingStyle == UITableViewCellEditingStyleInsert) {
        // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
    }   
}
*/

/*
// Override to support rearranging the table view.
- (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath
{
}
*/

/*
// Override to support conditional rearranging of the table view.
- (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Return NO if you do not want the item to be re-orderable.
    return YES;
}
*/

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/



- (void)addFooter {
    
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    CGFloat screenWidth = screenRect.size.width;
    CGFloat screenHeight = screenRect.size.height;
    
    NSInteger height = (screenHeight - (50*[_requests count]*2)-44);
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}

@end
