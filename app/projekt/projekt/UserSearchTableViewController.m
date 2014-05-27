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
    //_users = [[NSMutableArray alloc] initWithArray:@[@"A", @"B", @"C", @"D", @"E", @"F", @"G", @"H", @"I", @"J"]];
    _users = [[NSMutableArray alloc] init];
    _usersID = [[NSMutableArray alloc] init];

    
    [self addFooter];
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
        //[_users count];
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
    
   // [_users addObjectsFromArray:array];
    [theTextField resignFirstResponder];
    NSLog(@"%@",theTextField.text);
    [self.tableView reloadData];
    return YES;
}

/*
 -(BOOL)textViewShouldEndEditing:(UITextView *)textView{
 [textView resignFirstResponder];
 NSLog(@"%@",textView.text);
 return YES;
 }
 */

- (void)addFooter {
    UIView *v = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 320, 1)];
    v.backgroundColor = [UIColor clearColor];
    [self.tableView setTableFooterView:v];
}
@end
