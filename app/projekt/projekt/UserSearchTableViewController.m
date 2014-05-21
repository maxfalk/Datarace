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
@property (strong,nonatomic) NSMutableArray *filteredList;

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
    _users = [[NSMutableArray alloc] initWithArray:@[@"A", @"B", @"C", @"D", @"E", @"F", @"G", @"H", @"I", @"J"]];
    
    [NetworkConnectionClass searchForUsers];
}

- (void)filterContentForSearchText:(NSString*)searchText scope:(NSString*)scope {
    NSPredicate *resultPredicate = [NSPredicate predicateWithFormat:@"self contains[c] %@", searchText];
    searchResults = [_users filteredArrayUsingPredicate:resultPredicate];
}

-(BOOL)searchDisplayController:(UISearchDisplayController *)controller shouldReloadTableForSearchString:(NSString *)searchString {
    [self filterContentForSearchText:searchString
                               scope:[[self.searchDisplayController.searchBar scopeButtonTitles]
                                      objectAtIndex:[self.searchDisplayController.searchBar
                                                     selectedScopeButtonIndex]]];
    
    return YES;
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


@end
