-- MySQL dump 10.13  Distrib 5.5.35, for debian-linux-gnu (armv7l)
--
-- Host: localhost    Database: databasev2
-- ------------------------------------------------------
-- Server version	5.5.35-0+wheezy1

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
) ENGINE=InnoDB AUTO_INCREMENT=591 DEFAULT CHARSET=latin1 COMMENT='holds the gps coord for diffrent users in diffrent matches.';
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
) ENGINE=InnoDB AUTO_INCREMENT=6041 DEFAULT CHARSET=latin1 COMMENT='Keeps records of when a users is logged in and how long.';
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
) ENGINE=InnoDB AUTO_INCREMENT=253 DEFAULT CHARSET=latin1 COMMENT='Table to hold the matches made by users.';
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
) ENGINE=InnoDB AUTO_INCREMENT=476 DEFAULT CHARSET=latin1 COMMENT='Holds the users that have run a match.';
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
) ENGINE=InnoDB AUTO_INCREMENT=1406 DEFAULT CHARSET=latin1 COMMENT='Table holds requests made by users.';
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
) ENGINE=InnoDB AUTO_INCREMENT=2780 DEFAULT CHARSET=latin1 COMMENT='Holds the users to are connected to the different requests.';
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
  `averageDistance` int(11) NOT NULL,
  `averageSpeed` int(11) NOT NULL,
  `requests` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=16 DEFAULT CHARSET=latin1;
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
) ENGINE=InnoDB AUTO_INCREMENT=4341 DEFAULT CHARSET=latin1 COMMENT='Holds diffrent users passwords and information.	';
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2014-05-27 16:17:33
