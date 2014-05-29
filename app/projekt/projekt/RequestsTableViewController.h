//
//  RequestsTableViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 27/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "RequestTableViewCell.h"
#import "NetworkConnectionClass.h"
#import "CWStatusBarNotification.h"

@interface RequestsTableViewController : UITableViewController <UIAlertViewDelegate>

@property (strong, nonatomic) CWStatusBarNotification *notification;

@end
