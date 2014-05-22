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
        return 0;
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
        //cell.textLabel.text = [_users objectAtIndex:indexPath.row];
    }
    
    return cell;
}

- (BOOL)textFieldShouldReturn:(UITextField *)theTextField {
    
    //dispatch_async( dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Add code here to do background processing
      userArray *array = [NetworkConnectionClass searchForUsers:theTextField.text];
        
       
        
        
        //dispatch_async( dispatch_get_main_queue(), ^{
            // Add code here to update the UI/send notifications based on the
            // results of the background processing
          //  [self.tableView reloadData];
       // });
   // });
    
    [theTextField resignFirstResponder];
    NSLog(@"%@",theTextField.text);
    
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
