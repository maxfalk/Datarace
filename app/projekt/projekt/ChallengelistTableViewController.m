//
//  ChallengelistTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 23/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "ChallengelistTableViewController.h"

@interface ChallengelistTableViewController ()
@property (strong, nonatomic) NSMutableArray *competitors;

@end

@implementation ChallengelistTableViewController

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
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
    
    [self.tableView registerClass:[RMSwipeTableViewCelliOS7UIDemoTableViewCell class] forCellReuseIdentifier:@"Cell"];
    [self.tableView setRowHeight:77];self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];
    
    [self.navigationItem setTitle:NSLocalizedString(@"Messages", nil)];
    
    _competitors = [[NSMutableArray alloc] initWithArray:@[@"Babak Toghiani-Rizi", @"Marina Jaksic", @"Max Falk Nilsson", @"Max Reeves"]];
    
    [self addFooter];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    // Return the number of rows in the section.
    return [_competitors count];
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    static NSString *CellIdentifier = @"Cell";
    RMSwipeTableViewCelliOS7UIDemoTableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
    //CustomCell *cell = [[CustomCell alloc] initWithFrame:CGRectZero];
    
    // Configure the cell...
    //cell.primaryLabel.text  = [_competitors objectAtIndex:indexPath.row];
    
    cell.textLabel.text = [_competitors objectAtIndex:indexPath.row];
    cell.detailTextLabel.text = @"TEST";
    cell.selectionStyle = UITableViewCellSelectionStyleGray;
    cell.delegate = self;
    cell.demoDelegate = self;
    return cell;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    if (self.selectedIndexPath.row != indexPath.row) {
        [self.tableView deselectRowAtIndexPath:indexPath animated:YES];
        [self resetSelectedCell];
    }
    if (self.selectedIndexPath.row == indexPath.row) {
        [self.tableView deselectRowAtIndexPath:indexPath animated:YES];
        [self resetSelectedCell];
    }
}

-(void)scrollViewWillBeginDragging:(UIScrollView *)scrollView {
    if (self.selectedIndexPath) {
        [self resetSelectedCell];
    }
}

#pragma mark - RMSwipeTableViewCelliOS7UIDemoTableViewCell delegate method

-(void)swipeTableViewCellDidDelete:(RMSwipeTableViewCelliOS7UIDemoTableViewCell *)swipeTableViewCell {
    NSIndexPath *indexPath = [self.tableView indexPathForCell:swipeTableViewCell];
    [swipeTableViewCell resetContentView];
    [self.tableView beginUpdates];
    //[self.tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationAutomatic];
    //[self.tableView endUpdates];
    if ([_competitors count]) {
        //UIBarButtonItem *barButtonItem = [[UIBarButtonItem alloc] initWithTitle:NSLocalizedString(@"Reset", nil) style:UIBarButtonItemStylePlain target:self action:@selector(resetDemo)];
        //[self.navigationItem setRightBarButtonItem:barButtonItem animated:YES];
        [self performSegueWithIdentifier:@"challenge" sender:self];
    }
}

#pragma mark - RMSwipeTableViewCell delegate methods

-(void)swipeTableViewCellDidStartSwiping:(RMSwipeTableViewCell *)swipeTableViewCell {
    NSIndexPath *indexPathForCell = [self.tableView indexPathForCell:swipeTableViewCell];
    if (self.selectedIndexPath.row != indexPathForCell.row) {
        [self resetSelectedCell];
    }
}

-(void)performSegueWithIdentifier:(NSString *)identifier sender:(id)sender {
    
}

-(void)resetSelectedCell {
    RMSwipeTableViewCelliOS7UIDemoTableViewCell *cell = (RMSwipeTableViewCelliOS7UIDemoTableViewCell*)[self.tableView cellForRowAtIndexPath:self.selectedIndexPath];
    [cell resetContentView];
    self.selectedIndexPath = nil;
    cell.selectionStyle = UITableViewCellSelectionStyleGray;
}

-(void)swipeTableViewCellWillResetState:(RMSwipeTableViewCell *)swipeTableViewCell fromPoint:(CGPoint)point animation:(RMSwipeTableViewCellAnimationType)animation velocity:(CGPoint)velocity {
    if (velocity.x <= -500) {
        self.selectedIndexPath = [self.tableView indexPathForCell:swipeTableViewCell];
        swipeTableViewCell.shouldAnimateCellReset = NO;
        swipeTableViewCell.selectionStyle = UITableViewCellSelectionStyleNone;
        NSTimeInterval duration = MAX(-point.x / ABS(velocity.x), 0.10f);
        [UIView animateWithDuration:duration
                              delay:0
                            options:UIViewAnimationOptionCurveLinear
                         animations:^{
                             swipeTableViewCell.contentView.frame = CGRectOffset(swipeTableViewCell.contentView.bounds, point.x - (ABS(velocity.x) / 150), 0);
                         }
                         completion:^(BOOL finished) {
                             [UIView animateWithDuration:duration
                                                   delay:0
                                                 options:UIViewAnimationOptionCurveEaseOut
                                              animations:^{
                                                  swipeTableViewCell.contentView.frame = CGRectOffset(swipeTableViewCell.contentView.bounds, -100, 0);
                                              }
                                              completion:^(BOOL finished) {
                                              }];
                         }];
    }
    
    
    // The below behaviour is not normal as of iOS 7 beta seed 1
    // for Messages.app, but it is for Mail.app.
    // The user has to pan/swipe with a certain amount of velocity
    // before the cell goes to delete-state. If the user just pans
    // above the threshold for the button but without enough velocity,
    // the cell will reset.
    // Mail.app will, however allow for the cell to reveal the button
    // even if the velocity isn't high, but the pan translation is
    // above the threshold. I am assuming it'll get more consistent
    // in later seed of the iOS 7 beta
    /*
     else if (velocity.x > -500 && point.x < -80) {
     self.selectedIndexPath = [self.tableView indexPathForCell:swipeTableViewCell];
     swipeTableViewCell.shouldAnimateCellReset = NO;
     swipeTableViewCell.selectionStyle = UITableViewCellSelectionStyleNone;
     NSTimeInterval duration = MIN(-point.x / ABS(velocity.x), 0.15f);
     [UIView animateWithDuration:duration
     animations:^{
     swipeTableViewCell.contentView.frame = CGRectOffset(swipeTableViewCell.contentView.bounds, -80, 0);
     }];
     }
     */
}

-(void)resetDemo {
    _competitors = nil;
    [self.tableView reloadData];
    [self.navigationItem setRightBarButtonItem:nil animated:YES];
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


- (UITableViewCellEditingStyle)tableView:(UITableView *)aTableView editingStyleForRowAtIndexPath:(NSIndexPath *)indexPath
{
    // Detemine if it's in editing mode
    if (self.editing)
    {
        return UITableViewCellEditingStyleDelete;
    }
    
    return UITableViewCellEditingStyleNone;
}

*/



- (void)addFooter {
    
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    CGFloat screenWidth = screenRect.size.width;
    CGFloat screenHeight = screenRect.size.height;
    
    NSInteger height = (screenHeight - (50*[_competitors count]*2)-44);
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor whiteColor];
    [self.tableView setTableFooterView:v];
}


@end
