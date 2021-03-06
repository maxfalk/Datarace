-- MySQL dump 10.13  Distrib 5.5.37, for debian-linux-gnu (i686)
--
-- Host: localhost    Database: databasev2
-- ------------------------------------------------------
-- Server version	5.5.37-0ubuntu0.12.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `tGps`
--

DROP TABLE IF EXISTS `tGps`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tGps` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `matchParticipant` int(11) NOT NULL,
  `longitude` double NOT NULL,
  `latitude` double NOT NULL,
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tGpsMatchParForeignKey_idx` (`matchParticipant`),
  CONSTRAINT `tGpsMatchParForeignKey` FOREIGN KEY (`matchParticipant`) REFERENCES `tMatchParticipant` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=5035 DEFAULT CHARSET=latin1 COMMENT='holds the gps coord for diffrent users in diffrent matches.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tLoginLog`
--

DROP TABLE IF EXISTS `tLoginLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tLoginLog` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userId` int(11) NOT NULL,
  `login` datetime NOT NULL,
  `logout` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `tLoginLogUserIdForeignKey_idx` (`userId`),
  CONSTRAINT `tLoginLogUserIdForeignKey` FOREIGN KEY (`userId`) REFERENCES `tUsers` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=11333 DEFAULT CHARSET=latin1 COMMENT='Keeps records of when a users is logged in and how long.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tMatch`
--

DROP TABLE IF EXISTS `tMatch`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tMatch` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `requestId` int(11) NOT NULL,
  `time` datetime NOT NULL,
  `winnerUserId` int(11) NOT NULL,
  `state` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tMatchRequestIdForeignKey_idx` (`requestId`),
  KEY `tMatchwinnerUserIdForeignKey_idx` (`winnerUserId`),
  CONSTRAINT `tMatchRequestIdForeignKey` FOREIGN KEY (`requestId`) REFERENCES `tRequest` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=1840 DEFAULT CHARSET=latin1 COMMENT='Table to hold the matches made by users.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tMatchParticipant`
--

DROP TABLE IF EXISTS `tMatchParticipant`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tMatchParticipant` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `requestedUserId` int(11) NOT NULL,
  `matchId` int(11) NOT NULL,
  `state` int(11) NOT NULL,
  `date` datetime NOT NULL,
  `time` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `tMatchParticipantUserIdForeignKey_idx` (`requestedUserId`),
  KEY `tMatchParticipantMatchIdForeginKey` (`matchId`),
  CONSTRAINT `tMatchParticipantMatchIdForeginKey` FOREIGN KEY (`matchId`) REFERENCES `tMatch` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `tMatchParticipantUserIdForeignKey` FOREIGN KEY (`requestedUserId`) REFERENCES `tRequestedUsers` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=1953 DEFAULT CHARSET=latin1 COMMENT='Holds the users that have run a match.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tRequest`
--

DROP TABLE IF EXISTS `tRequest`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tRequest` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userId` int(11) NOT NULL,
  `distance` int(11) NOT NULL,
  `time` datetime NOT NULL,
  `state` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tRequestUserIdForeignKey_idx` (`userId`),
  CONSTRAINT `tRequestUserIdForeignKey` FOREIGN KEY (`userId`) REFERENCES `tUsers` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=2198 DEFAULT CHARSET=latin1 COMMENT='Table holds requests made by users.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tRequestedUsers`
--

DROP TABLE IF EXISTS `tRequestedUsers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tRequestedUsers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `requestId` int(11) NOT NULL,
  `userId` int(11) NOT NULL,
  `distance` int(11) NOT NULL,
  `state` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tRequestedUsersRequestIdForeignKey_idx` (`requestId`),
  KEY `tRequestedUsersUserIdForeignKey_idx` (`userId`),
  CONSTRAINT `tRequestedUsersRequestIdForeignKey` FOREIGN KEY (`requestId`) REFERENCES `tRequest` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `tRequestedUsersUserIdForeignKey` FOREIGN KEY (`userId`) REFERENCES `tUsers` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=4395 DEFAULT CHARSET=latin1 COMMENT='Holds the users to are connected to the different requests.';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tUserStatistics`
--

DROP TABLE IF EXISTS `tUserStatistics`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tUserStatistics` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userId` int(11) NOT NULL,
  `userName` varchar(50) NOT NULL,
  `matches` int(11) NOT NULL,
  `wins` int(11) NOT NULL,
  `averageDistance` double NOT NULL,
  `averageSpeed` double NOT NULL,
  `requests` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=600 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `tUsers`
--

DROP TABLE IF EXISTS `tUsers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tUsers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userName` varchar(50) NOT NULL,
  `email` varchar(50) NOT NULL,
  `password` varchar(100) NOT NULL,
  `salt` varchar(100) NOT NULL,
  `registerDate` datetime NOT NULL,
  `state` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=9310 DEFAULT CHARSET=latin1 COMMENT='Holds diffrent users passwords and information.	';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping events for database 'databasev2'
--
/*!50106 SET @save_time_zone= @@TIME_ZONE */ ;
/*!50106 DROP EVENT IF EXISTS `cuser_cleanup` */;
DELIMITER ;;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;;
/*!50003 SET character_set_client  = utf8 */ ;;
/*!50003 SET character_set_results = utf8 */ ;;
/*!50003 SET collation_connection  = utf8_general_ci */ ;;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;;
/*!50003 SET sql_mode              = '' */ ;;
/*!50003 SET @saved_time_zone      = @@time_zone */ ;;
/*!50003 SET time_zone             = 'SYSTEM' */ ;;
/*!50106 CREATE*/ /*!50117 DEFINER=`root`@`localhost`*/ /*!50106 EVENT `cuser_cleanup` ON SCHEDULE EVERY 1 HOUR STARTS '2014-06-03 19:30:00' ON COMPLETION NOT PRESERVE ENABLE DO BEGIN
	
	CALL user_cleanup();
	    
	END */ ;;
/*!50003 SET time_zone             = @saved_time_zone */ ;;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;;
/*!50003 SET character_set_client  = @saved_cs_client */ ;;
/*!50003 SET character_set_results = @saved_cs_results */ ;;
/*!50003 SET collation_connection  = @saved_col_connection */ ;;
/*!50106 DROP EVENT IF EXISTS `user_stats` */;;
DELIMITER ;;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;;
/*!50003 SET character_set_client  = utf8 */ ;;
/*!50003 SET character_set_results = utf8 */ ;;
/*!50003 SET collation_connection  = utf8_general_ci */ ;;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;;
/*!50003 SET sql_mode              = '' */ ;;
/*!50003 SET @saved_time_zone      = @@time_zone */ ;;
/*!50003 SET time_zone             = 'SYSTEM' */ ;;
/*!50106 CREATE*/ /*!50117 DEFINER=`root`@`localhost`*/ /*!50106 EVENT `user_stats` ON SCHEDULE EVERY 1 HOUR STARTS '2014-06-03 11:00:00' ON COMPLETION NOT PRESERVE ENABLE DO BEGIN
	CALL insert_user_stats() ;
	END */ ;;
/*!50003 SET time_zone             = @saved_time_zone */ ;;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;;
/*!50003 SET character_set_client  = @saved_cs_client */ ;;
/*!50003 SET character_set_results = @saved_cs_results */ ;;
/*!50003 SET collation_connection  = @saved_col_connection */ ;;
DELIMITER ;
/*!50106 SET TIME_ZONE= @save_time_zone */ ;

--
-- Dumping routines for database 'databasev2'
--
/*!50003 DROP PROCEDURE IF EXISTS `all_match_stats` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
CREATE DEFINER=`root`@`localhost` PROCEDURE `all_match_stats`(
	IN IN_UserId INT
)
BEGIN

-- tid, win, distance, avergeHastighet
CREATE temporary table tmp_all_match_stats(
	userId INT,
	userName VARCHAR(50),
	time INT,
	winner INT,
	distance INT,
	averageSpeed DOUBLE,
	state INT
);

create temporary table tmp_all_match_stats_ids(
	matchPartId INT
);

INSERT INTO tmp_all_match_stats_ids
SELECT t2.id
from
	tMatch t1 inner join
	tMatchParticipant t2 on t1.id = t2.matchId inner join
	tRequestedUsers t3 on t2.requestedUserId = t3.id
WHERE
	t3.userId = IN_UserId and
	t2.state != 0;


INSERT into tmp_all_match_stats
SELECT t3.userId, IFNULL(t6.userName, "undefined"), t2.time, t1.winnerUserId as winner, t4.distance, IFNULL(t4.distance/(t2.time/3600),0) as averageSpeed, t2.state
FROM
	tMatch t1 inner join
	tMatchParticipant t2 on t1.id = t2.matchId inner join
	tRequestedUsers t3 on t2.requestedUserId = t3.id inner join
	tRequest t4 on t3.requestId = t4.id inner join
	tmp_all_match_stats_ids t5 on t2.id = t5.matchPartId left join
	tRequestedUsers t8 on t3.requestId = t8.requestId and t3.userId != t8.userId left join
	tUsers t6 on t8.userId = t6.id
LIMIT 30;



select *
from tmp_all_match_stats;


drop table tmp_all_match_stats;
drop table tmp_all_match_stats_ids;




END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `delete_user` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
CREATE DEFINER=`root`@`localhost` PROCEDURE `delete_user`(
	IN IN_Userid int(32)
)
BEGIN

UPDATE tUsers t1 SET t1.state = 1 WHERE t1.Id = IN_Userid; 
	

END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `insert_user_stats` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
CREATE DEFINER=`root`@`localhost` PROCEDURE `insert_user_stats`(
	
)
BEGIN


CREATE temporary table tmpMatchStats(
	userId INT,
	userName VARCHAR(100),
	averageDistance DOUBLE default 0,
	averageSpeed DOUBLE default 0,
	wins INT default 0,
	matches INT default 0,
	requests INT default 0

);


insert into tmpMatchStats(userId, userName)
select t1.id, t1.userName
from tUsers t1
where
	t1.state = 0;


update tmpMatchStats t1 inner join
(
select 
	t1.id, 
	t1.userName, 
	IFNULL(sum(t4.distance)/count(t2.id),0) as averageDistance, 
	count(t2.id) as matches, 
	SUM(case t5.winnerUserId WHEN t1.id THEN 1 ELSE 0 END) as wins,
	IFNULL( sum( case t2.state when 1 then t4.distance else 0 END),0) / 
	SUM( IFNULL( (case t2.state when 1 then t2.time else 0 end) / 3600, 0)) as averageSpeed
from 	tUsers t1 inner join
		tRequestedUsers t3 on t3.userId = t1.id inner join
		tMatchParticipant t2 on t2.requestedUserId = t3.id  inner join	
		tRequest t4 on t3.requestId = t4.id inner join
		tMatch t5 on t5.requestId = t4.id
where
	t5.state = 1
group by 
	t1.id, t1.userName) t2 on t1.userId = t2.id
SET t1.averageDistance = t2.averageDistance, t1.matches = t2.matches, t1.wins = t2.wins, t1.averageSPeed = t2.averageSpeed;


update 
	tmpMatchStats t1 inner join
	(
	select t1.userId, count(t1.id) as requests
	from
	tRequestedUsers t1
	group by t1.userId
) t2 on t1.userId = t2.userId
set t1.requests = t2.requests;


insert into tUserStatistics(userId, userName, averageDistance, averageSpeed,wins, matches, requests)
select t1.*
from tmpMatchStats t1 left join
	tUserStatistics t2 on t1.userId = t2.userId
where
	t2.userId is null;

update tUserStatistics t1 inner join
	tmpMatchStats t2 on t1.userId = t2.userId
set t1.averageSpeed = t2.averageSpeed, t1.averageDistance = t2.averageDistance, 
	t1.matches = t2.matches, t1.requests = t2.requests, t1.wins = t2.wins;


drop table tmpMatchStats;


END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `match_stats` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
CREATE DEFINER=`root`@`localhost` PROCEDURE `match_stats`(
	IN IN_MatchId INT
)
BEGIN

-- tid, win, distance, avergeHastighet
CREATE temporary table tmp_match_stats(
	userId INT,
	userName VARCHAR(50),
	time INT,
	winner INT,
	distance INT,
	averageSpeed DOUBLE,
	state INT
);

create temporary table tmp_match_stats_ids(
	matchPartId INT
);

INSERT INTO tmp_match_stats_ids
SELECT t2.id
from
	tMatch t1 inner join
	tMatchParticipant t2 on t1.id = t2.matchId
WHERE
	t1.id = IN_MatchId;


INSERT into tmp_match_stats
SELECT t3.userId, t6.userName, IFNULL(t2.time,0), t1.winnerUserId as winner, t4.distance, IFNULL(t4.distance/(t2.time/3600),0) as averageSpeed, t2.state
FROM
	tMatch t1 inner join
	tMatchParticipant t2 on t1.id = t2.matchId inner join
	tRequestedUsers t3 on t2.requestedUserId = t3.id inner join
	tRequest t4 on t3.requestId = t4.id inner join
	tmp_match_stats_ids t5 on t2.id = t5.matchPartId inner join
	tUsers t6 on t3.userId = t6.id
LIMIT 30;



select *
from tmp_match_stats;


drop table tmp_match_stats;
drop table tmp_match_stats_ids;




END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 DROP PROCEDURE IF EXISTS `user_cleanup` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
CREATE DEFINER=`root`@`localhost` PROCEDURE `user_cleanup`()
BEGIN

CREATE temporary TABLE tmp_ids(
	requestID INT,
	requestedUserId INT,
	matchId INT,
	matchParId INT

);

CREATE temporary TABLE tmp_users(
	userID INT

);

INSERT INTO tmp_users
SELECT t1.id
from
	tUsers t1
where
	t1.state = 1;


INSERT INTO tmp_ids
SELECT t1.id as requestId, t2.id as requestedUserId, t3.id as matchId, t4.id as matchParId 
FROM 
	tmp_users t0 inner join
	tRequest t1 on t0.userId = t1.userId left join
	tRequestedUsers t2 on t1.id = t2.requestId left join
	tMatch t3 on t1.id = t3.requestId left join
	tMatchParticipant t4 on t3.id = t4.matchId; 
	

INSERT INTO tmp_ids(requestedUserId, MatchParId)
SELECT t2.id as requestedUserId , t3.id
FROM 
	tmp_users t0 inner join
	tRequestedUsers t2 on t0.userId = t2.userId left join
	tMatchParticipant t3 on t2.id = t3.requestedUserId; 


DELETE t1.*
FROM 
	tmp_users t0 inner join
	tUserStatistics t1  on t0.userId = t1.userId;

DELETE t1.*
FROM tGps t1 inner join
tmp_ids t2 on t1.matchparticipant = t2.matchParId;

DELETE t1.*
FROM
	tMatchParticipant t1 inner join
	tmp_ids t2 on t2.matchParId = t1.id;


DELETE t1.*
FROM tRequestedUsers t1 inner join
	tmp_ids t2 on t1.id = t2.requestedUserId;


DELETE t1.* 
FROM tMatch t1 inner join
tmp_ids t2 on t1.id = t2.matchId ;

DELETE t1.*
FROM tRequest t1 inner join
tmp_ids t2 on t1.id = t2.requestId;



DELETE t1.*
FROM 
	tmp_users t0 inner join
	tLoginLog t1 on t0.userId = t1.userId;

DELETE t1.*
FROM 
	tmp_users t0 inner join
	tUsers t1 on t0.userId = t1.Id;

drop table tmp_ids;
drop table tmp_users;


END ;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2014-06-04 10:58:36
