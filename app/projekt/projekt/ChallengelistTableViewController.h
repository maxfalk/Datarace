//
//  ChallengelistTableViewController.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 23/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "CustomCell.h"
#import "RMSwipeTableViewCelliOS7UIDemoTableViewCell.h"

@interface ChallengelistTableViewController : UITableViewController <RMSwipeTableViewCellDelegate, RMSwipeTableViewCelliOS7UIDemoTableViewCellDelegate>

@property (nonatomic, strong) NSIndexPath *selectedIndexPath;


@end
