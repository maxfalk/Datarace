//
//  UserSearchTableViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 20/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "UserSearchTableViewController.h"

@interface UserSearchTableViewController ()

@property (nonatomic, strong) NSMutableArray *users;
@property (nonatomic, strong) NSMutableArray *usersID;
@property (strong,nonatomic) NSMutableArray *arrayWithUsers;
@property (weak, nonatomic) IBOutlet UITextField *searchTextField;

@end

@implementation UserSearchTableViewController {
    NSArray *searchResults;
}

- (id)initWithStyle:(UITableViewStyle)style {
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    _searchTextField.delegate = self;
     _arrayWithUsers = [[NSMutableArray alloc] init];
    _users = [[NSMutableArray alloc] init];
    _usersID = [[NSMutableArray alloc] init];
    self.tableView.backgroundColor = [UIColor colorWithRed:0.61 green:0.73 blue:0.81 alpha:1];

    [self addFooter];
}

-(void)viewDidAppear:(BOOL)animated {
    if ([_users count] == 0) {
        UIImageView *tempImageView = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"nouserfound"]];
        self.tableView.backgroundView = tempImageView;
    }
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    if (tableView == self.searchDisplayController.searchResultsTableView) {
        return [searchResults count];
    } else {
        return [_users count];
    }
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    CustomCell *cell = [[CustomCell alloc] initWithFrame:CGRectZero];
    NSString *temp;
    
    if (tableView == self.searchDisplayController.searchResultsTableView) {
        temp = [searchResults objectAtIndex:indexPath.row];
        cell.textLabel.text = temp;
    } else {
        cell.textLabel.text = [_users objectAtIndex:indexPath.row];
    }
    
    return cell;
}

- (BOOL)textFieldShouldReturn:(UITextField *)theTextField {
    

    //[_users addObjectsFromArray:
   userArray *result = [NetworkConnectionClass searchForUsers:theTextField.text];
    [_users removeAllObjects];
    [_usersID removeAllObjects];

    int numberOfUsers = ((result->length)/(sizeof(userInfo)));
    for (int i = 0; i < numberOfUsers; i++) {
        
        [_users addObject:[NSString stringWithFormat:@"%s",result->array[i].username]];
        
        [_usersID addObject:[NSNumber numberWithInt:result->array[i].userID]];
    
    }
    
    [theTextField resignFirstResponder];
    [self.tableView reloadData];
    
    if ([_users count] == 0) {
        UIImageView *tempImageView = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"nouserfound"]];
        self.tableView.backgroundView = tempImageView;
    } else {
        self.tableView.backgroundView.hidden = YES;
    }
    return YES;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    
   // UITableViewCell *cell = [tableView cellForRowAtIndexPath:indexPath];
    
    [self performSegueWithIdentifier:@"challenge" sender:indexPath];
}

-(void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    
    if ([segue.identifier isEqualToString:@"challenge"]) {
        NSIndexPath *indexPath = sender;
        
        ChallengeViewController *class = (ChallengeViewController *) [segue destinationViewController];
        class.challengerUsername = [_users objectAtIndex:indexPath.row];
        NSLog(@"challengerID: %ld", (long)[[_usersID objectAtIndex:indexPath.row] integerValue]);
        class.challengerID = (long)[[_usersID objectAtIndex:indexPath.row] integerValue];
    
    }
}


- (void)addFooter {
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}

@end
