//
//  RequestTableViewCell.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 27/05/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#import "RequestTableViewCell.h"

@implementation RequestTableViewCell

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)awakeFromNib
{
    // Initialization code
}

- (void)setSelected:(BOOL)selected animated:(BOOL)animated
{
    [super setSelected:selected animated:animated];

    // Configure the view for the selected state
}

- (id)initWithFrame:(CGRect)frame reuseIdentifier:(NSString *)reuseIdentifier {
    if (self = [super initWithFrame:frame reuseIdentifier:reuseIdentifier]) {
        // Initialization code
        //_label = [[UILabel alloc]init];
        
        _usernameLabel.textAlignment = UITextAlignmentLeft;
        _usernameLabel.font = [UIFont systemFontOfSize:17];
        _usernameLabel.lineBreakMode = NSLineBreakByWordWrapping;
        
        _distanceLabel.textAlignment = UITextAlignmentLeft;
        _distanceLabel.font = [UIFont systemFontOfSize:14];
        _distanceLabel.lineBreakMode = NSLineBreakByWordWrapping;
        /*
         _imageView.frame = CGRectMake(0,0, 50,50);
         [self.contentView addSubView:_imageView];
         */
        [self.contentView addSubview:_usernameLabel];
        [self.contentView addSubview:_distanceLabel];
        
    }
    return self;
}

- (void)layoutSubviews {
    [super layoutSubviews];
    CGRect contentRect = self.contentView.bounds;
    CGFloat boundsX = contentRect.origin.x;
    CGRect frame;
    
    
    //frame= CGRectMake(20,11,160,21);
    //_distance.frame = frame;
    
    //frame= CGRectMake(boundsX+15 ,0, 205, 44);
    //s _headerLabel.frame = frame;
    
    
    
}


@end
