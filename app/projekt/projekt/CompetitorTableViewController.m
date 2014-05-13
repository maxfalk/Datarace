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
@property (nonatomic, strong) UIColor *green;
@property (nonatomic, strong) UIColor *red;

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
    
    self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];
    //fetch requests from server
    
    _competitorsReady = [[NSMutableArray alloc] initWithArray:@[@"Max Falk Nilsson", @"Max Reeves", @"Hallå Därsson", @"Okreativ Kille"]];
    _competitors = [[NSMutableArray alloc] initWithArray:@[@"Babak Toghiani-Rizi", @"Marina Jaksic", @"Namn Namnsson"]];
    
    _green = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
    _red = [UIColor colorWithRed:0.91 green:0.04 blue:0.09 alpha:1];
    
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
    
    [self addFooter];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    return 60;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 2;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section {
    
    if(section == 0) {
        return @"READY";
    } else {
        return @"Available competitors";
    }
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    if (section==0)
    {
        return [_competitorsReady count];
    }
    else {
        return [_competitors count];
    }
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
            cell. statusLabel.text = @"Pending";
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

- (void)addFooter {
    
    //CGRect screenRect = [[UIScreen mainScreen] bounds];
    //CGFloat screenHeight = screenRect.size.height;
    
    //NSInteger height = (screenHeight - (50*[_competitors count]*2)-44);
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}


- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    
    CustomCell *cell = (CustomCell *)[tableView cellForRowAtIndexPath:indexPath];
    [tableView deselectRowAtIndexPath:indexPath animated:YES];
    
    if (indexPath.section == 0) {
        if ([cell.statusLabel.text isEqualToString:@"READY"]) {
            NSLog(@"Pressed on a READY cell");
            [self performSegueWithIdentifier:@"startChallenge" sender:nil];

            
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
        
    }
}



//-(void)tableView(UITableView *) didsel

/*
 - (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
 {
 UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:<#@"reuseIdentifier"#> forIndexPath:indexPath];
 
 // Configure the cell...
 
 return cell;
 }
 */

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

@end
