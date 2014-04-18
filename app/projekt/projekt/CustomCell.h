//
//  CustomCell.h
//  projekt
//
//  Created by Babak Toghiani-Rizi on 17/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface CustomCell : UITableViewCell {
    
    UILabel *primaryLabel;
    UILabel *primaryLabelTwo;
    UILabel *distanceLabel;
    UIImageView *myImageView;
    UIImageView *mySecondImageView;
    
}

@property(nonatomic,retain)UILabel *primaryLabel;
@property(nonatomic,retain)UILabel *primaryLabelTwo;
@property(nonatomic,retain)UILabel *distanceLabel;
@property(nonatomic,retain)UIImageView *myImageView;
@property(nonatomic,retain)UIImageView *mySecondImageView;
@end