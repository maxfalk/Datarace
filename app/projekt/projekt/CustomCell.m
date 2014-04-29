//
//  CustomCell.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 17/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "CustomCell.h"


@implementation CustomCell

@synthesize primaryLabel, primaryLabelTwo, distanceLabel, myImageView, mySecondImageView, competitorLabel;

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)setSelected:(BOOL)selected animated:(BOOL)animated
{
    [super setSelected:selected animated:animated];
    
    // Configure the view for the selected state
}


- (id)initWithFrame:(CGRect)frame reuseIdentifier:(NSString *)reuseIdentifier {
    if (self = [super initWithFrame:frame reuseIdentifier:reuseIdentifier]) {
        // Initialization code
        primaryLabel = [[UILabel alloc]init];
        primaryLabel.textAlignment = UITextAlignmentLeft;
        primaryLabel.font = [UIFont boldSystemFontOfSize:18];
        primaryLabel.lineBreakMode = NSLineBreakByWordWrapping;
        
        competitorLabel = [[UILabel alloc]init];
        competitorLabel.textAlignment = UITextAlignmentLeft;
        competitorLabel.font = [UIFont systemFontOfSize:19];
        competitorLabel.lineBreakMode = NSLineBreakByWordWrapping;
        
        primaryLabelTwo = [[UILabel alloc]init];
        primaryLabelTwo.textAlignment = UITextAlignmentLeft;
        primaryLabelTwo.font = [UIFont systemFontOfSize:12];
        primaryLabelTwo.lineBreakMode = NSLineBreakByWordWrapping;
        
        distanceLabel = [[UILabel alloc]init];
        distanceLabel.textAlignment = UITextAlignmentLeft;
        distanceLabel.font = [UIFont boldSystemFontOfSize:20];
        distanceLabel.lineBreakMode = NSLineBreakByWordWrapping;

        
        myImageView = [[UIImageView alloc]init];
    
        mySecondImageView = [[UIImageView alloc]init];
        
        [self.contentView addSubview:primaryLabel];
        [self.contentView addSubview:primaryLabelTwo];
        [self.contentView addSubview:competitorLabel];
        [self.contentView addSubview:distanceLabel];
        [self.contentView addSubview:myImageView];
        [self.contentView addSubview:mySecondImageView];
        
        
    }
    return self;
}

- (void)layoutSubviews {
    [super layoutSubviews];
    CGRect contentRect = self.contentView.bounds;
    CGFloat boundsX = contentRect.origin.x;
    CGRect frame;
    frame= CGRectMake(boundsX+260 ,35, 34, 34);
    myImageView.frame = frame;
    
    frame= CGRectMake(boundsX+5 ,2, 50, 50);
    mySecondImageView.frame = frame;
    
    frame= CGRectMake(boundsX+15 ,15, 250, 30);
    competitorLabel.frame = frame;
    
    frame= CGRectMake(boundsX+15 ,10, 250, 20);
    primaryLabel.frame = frame;
    
    frame= CGRectMake(boundsX+160-35 ,30, 70, 25);
    primaryLabelTwo.frame = frame;
    
    frame= CGRectMake(boundsX+160-35 ,50, 70, 25);
    distanceLabel.frame = frame;

    
    
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return 50;
}

@end
