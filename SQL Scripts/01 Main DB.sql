-- MySQL Administrator dump 1.4
--
-- ------------------------------------------------------
-- Server version	5.1.34-community


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


--
-- Create schema delphims
--

CREATE DATABASE IF NOT EXISTS delphims;
USE delphims;

--
-- Definition of table `accounts`
--

DROP TABLE IF EXISTS `accounts`;
CREATE TABLE `accounts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(13) NOT NULL DEFAULT '',
  `password` varchar(128) NOT NULL DEFAULT '',
  `salt` varchar(32) DEFAULT NULL,
  `pin` varchar(4) DEFAULT NULL,
  `pic` varchar(13) NOT NULL DEFAULT '',
  `loggedin` tinyint(4) NOT NULL DEFAULT '0',
  `lastlogin` timestamp NULL DEFAULT NULL,
  `createdat` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `birthday` date NOT NULL DEFAULT '0000-00-00',
  `banned` tinyint(1) NOT NULL DEFAULT '0',
  `tempban` int(10) unsigned NOT NULL DEFAULT '0',
  `banreason` text,
  `gm` tinyint(1) NOT NULL DEFAULT '0',
  `email` tinytext,
  `emailcode` varchar(40) DEFAULT NULL,
  `lastpwemail` timestamp NOT NULL DEFAULT '2002-12-31 10:00:00',
  `storageMesos` int(11) DEFAULT '0',
  `storageSlots` int(11) DEFAULT '-1',
  `lastIp` varchar(30) DEFAULT NULL,
  `paypalNX` int(10) unsigned NOT NULL DEFAULT '0',
  `mPoints` int(10) unsigned NOT NULL DEFAULT '0',
  `cardNX` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`),
  KEY `ranking1` (`id`,`banned`,`gm`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `alliance`
--

DROP TABLE IF EXISTS `alliance`;
CREATE TABLE `alliance` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(13) NOT NULL,
  `notice` varchar(128) NOT NULL DEFAULT '',
  `capacity` int(10) unsigned NOT NULL DEFAULT '2',
  `rank_title1` varchar(45) NOT NULL DEFAULT 'Master',
  `rank_title2` varchar(45) NOT NULL DEFAULT 'Jr.Master',
  `rank_title3` varchar(45) NOT NULL DEFAULT 'Member',
  `rank_title4` varchar(45) NOT NULL DEFAULT 'Member',
  `rank_title5` varchar(45) NOT NULL DEFAULT 'Member',
  `guild1` int(10) NOT NULL DEFAULT '-1',
  `guild2` int(10) NOT NULL DEFAULT '-1',
  `guild3` int(10) NOT NULL DEFAULT '-1',
  `guild4` int(10) NOT NULL DEFAULT '-1',
  `guild5` int(10) NOT NULL DEFAULT '-1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `buddies`
--

DROP TABLE IF EXISTS `buddies`;
CREATE TABLE `buddies` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `characterid` int(11) NOT NULL,
  `buddyid` int(11) NOT NULL,
  `group` varchar(17) NOT NULL DEFAULT 'Default Group',
  `pending` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `buddies_ibfk_1` (`characterid`),
  CONSTRAINT `buddies_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `areainfo`
--

DROP TABLE IF EXISTS `areainfo`;
CREATE TABLE `areainfo` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `charid` int(11) NOT NULL,
  `infoid` int(11) NOT NULL,
  `area_data` varchar(120) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `cid` (`charid`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

--
-- Definition of table `characters`
--

DROP TABLE IF EXISTS `characters`;
CREATE TABLE `characters` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `accountid` int(11) NOT NULL DEFAULT '0',
  `world` int(11) NOT NULL DEFAULT '0',
  `name` varchar(13) NOT NULL DEFAULT '',
  `level` int(11) NOT NULL DEFAULT '0',
  `exp` int(11) NOT NULL DEFAULT '0',
  `str` int(11) NOT NULL DEFAULT '0',
  `dex` int(11) NOT NULL DEFAULT '0',
  `luk` int(11) NOT NULL DEFAULT '0',
  `int` int(11) NOT NULL DEFAULT '0',
  `hp` int(11) NOT NULL DEFAULT '0',
  `mp` int(11) NOT NULL DEFAULT '0',
  `maxhp` int(11) NOT NULL DEFAULT '0',
  `maxmp` int(11) NOT NULL DEFAULT '0',
  `meso` int(11) NOT NULL DEFAULT '0',
  `hpApUsed` int(11) NOT NULL DEFAULT '0',
  `mpApUsed` int(11) NOT NULL DEFAULT '0',
  `job` int(11) NOT NULL DEFAULT '0',
  `skincolor` int(11) NOT NULL DEFAULT '0',
  `gender` int(11) NOT NULL DEFAULT '0',
  `fame` int(11) NOT NULL DEFAULT '0',
  `hair` int(11) NOT NULL DEFAULT '0',
  `face` int(11) NOT NULL DEFAULT '0',
  `ap` int(11) NOT NULL DEFAULT '0',
  `sp` int(11) NOT NULL DEFAULT '0',
  `map` int(11) NOT NULL DEFAULT '0',
  `spawnpoint` int(11) NOT NULL DEFAULT '0',
  `party` int(11) NOT NULL DEFAULT '0',
  `buddyCapacity` int(11) NOT NULL DEFAULT '25',
  `createdate` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `rank` int(10) unsigned NOT NULL DEFAULT '1',
  `rankMove` int(11) NOT NULL DEFAULT '0',
  `jobRank` int(10) unsigned NOT NULL DEFAULT '1',
  `jobRankMove` int(11) NOT NULL DEFAULT '0',
  `guildid` int(10) unsigned NOT NULL DEFAULT '0',
  `guildrank` int(10) unsigned NOT NULL DEFAULT '5',
  `allianceRank` int(10) unsigned NOT NULL DEFAULT '5',
  `equipSlots` int(10) unsigned NOT NULL DEFAULT '24',
  `useSlots` int(10) unsigned NOT NULL DEFAULT '24',
  `setupSlots` int(10) unsigned NOT NULL DEFAULT '24',
  `etcSlots` int(10) unsigned NOT NULL DEFAULT '24',
  `storageSlots` int(10) unsigned NOT NULL DEFAULT '18',
  `hasMerchant` tinyint(1) NOT NULL DEFAULT '0',
  `familyId` int(11) NOT NULL DEFAULT '-1',
  `monsterbookcover` int(11) NOT NULL DEFAULT '0',
  `jaguar` tinyint(3) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `accountid` (`accountid`),
  KEY `party` (`party`),
  KEY `ranking1` (`level`,`exp`),
  KEY `ranking2` (`job`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `cooldowns`
--

DROP TABLE IF EXISTS `cooldowns`;
CREATE TABLE `cooldowns` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `charid` int(11) NOT NULL,
  `skillid` int(11) NOT NULL,
  `length` bigint(20) unsigned NOT NULL,
  `starttime` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `csgifts`
--

DROP TABLE IF EXISTS `csgifts`;
CREATE TABLE `csgifts` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `accountid` int(10) unsigned NOT NULL DEFAULT '0',
  `itemid` int(10) unsigned NOT NULL DEFAULT '0',
  `sn` int(10) unsigned NOT NULL DEFAULT '0',
  `quantity` int(10) unsigned NOT NULL DEFAULT '1',
  `expiredate` datetime NOT NULL,
  `sender` varchar(45) NOT NULL,
  `message` text NOT NULL,
  `isRing` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=12 DEFAULT CHARSET=latin1;

--
-- Definition of table `csinventory`
--

DROP TABLE IF EXISTS `csinventory`;
CREATE TABLE `csinventory` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `accountid` int(10) unsigned NOT NULL,
  `uniqueid` int(10) unsigned NOT NULL,
  `itemid` int(10) unsigned NOT NULL,
  `sn` int(10) unsigned NOT NULL,
  `quantity` int(10) unsigned NOT NULL,
  `sender` varchar(45) NOT NULL,
  `message` text NOT NULL,
  `expiredate` datetime NOT NULL,
  `gift` tinyint(1) unsigned NOT NULL,
  `isRing` tinyint(1) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=328 DEFAULT CHARSET=latin1;

--
-- Definition of table `famelog`
--

DROP TABLE IF EXISTS `famelog`;
CREATE TABLE `famelog` (
  `famelogid` int(11) NOT NULL AUTO_INCREMENT,
  `characterid` int(11) NOT NULL DEFAULT '0',
  `characterid_to` int(11) NOT NULL DEFAULT '0',
  `when` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`famelogid`),
  KEY `characterid` (`characterid`),
  CONSTRAINT `famelog_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `family_character`
--

DROP TABLE IF EXISTS `family_character`;
CREATE TABLE `family_character` (
  `cid` int(11) NOT NULL,
  `rank` int(11) NOT NULL,
  `reputation` int(11) NOT NULL,
  `todaysrep` int(11) NOT NULL,
  `totaljuniors` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  `juniorsadded` int(11) NOT NULL,
  `totalreputation` int(11) NOT NULL,
  PRIMARY KEY (`cid`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `guilds`
--

DROP TABLE IF EXISTS `guilds`;
CREATE TABLE `guilds` (
  `guildid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `allianceId` int(10) unsigned DEFAULT '0',
  `leader` int(10) unsigned NOT NULL DEFAULT '0',
  `GP` int(10) unsigned NOT NULL DEFAULT '0',
  `logo` int(10) unsigned DEFAULT NULL,
  `logoColor` smallint(5) unsigned NOT NULL DEFAULT '0',
  `name` varchar(45) NOT NULL,
  `rank1title` varchar(45) NOT NULL DEFAULT 'Master',
  `rank2title` varchar(45) NOT NULL DEFAULT 'Jr. Master',
  `rank3title` varchar(45) NOT NULL DEFAULT 'Member',
  `rank4title` varchar(45) NOT NULL DEFAULT 'Member',
  `rank5title` varchar(45) NOT NULL DEFAULT 'Member',
  `capacity` int(10) unsigned NOT NULL DEFAULT '10',
  `logoBG` int(10) unsigned DEFAULT NULL,
  `logoBGColor` smallint(5) unsigned NOT NULL DEFAULT '0',
  `notice` varchar(101) DEFAULT NULL,
  `signature` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`guildid`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;

--
-- Definition of table `equipment`
--

DROP TABLE IF EXISTS `equipment`;
CREATE TABLE `equipment` (
  `inventoryequipmentid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `inventoryitemid` int(10) unsigned NOT NULL DEFAULT '0',
  `upgradeslots` int(11) NOT NULL DEFAULT '0',
  `level` int(11) NOT NULL DEFAULT '0',
  `str` int(11) NOT NULL DEFAULT '0',
  `dex` int(11) NOT NULL DEFAULT '0',
  `int` int(11) NOT NULL DEFAULT '0',
  `luk` int(11) NOT NULL DEFAULT '0',
  `hp` int(11) NOT NULL DEFAULT '0',
  `mp` int(11) NOT NULL DEFAULT '0',
  `watk` int(11) NOT NULL DEFAULT '0',
  `matk` int(11) NOT NULL DEFAULT '0',
  `wdef` int(11) NOT NULL DEFAULT '0',
  `mdef` int(11) NOT NULL DEFAULT '0',
  `acc` int(11) NOT NULL DEFAULT '0',
  `avoid` int(11) NOT NULL DEFAULT '0',
  `hands` int(11) NOT NULL DEFAULT '0',
  `speed` int(11) NOT NULL DEFAULT '0',
  `jump` int(11) NOT NULL DEFAULT '0',
  `ringid` int(11) NOT NULL DEFAULT '-1',
  `locked` int(11) NOT NULL DEFAULT '0',
  `vicious` int(11) unsigned NOT NULL DEFAULT '0',
  `flag` int(11) NOT NULL DEFAULT '0',
  `itemexp` int(11) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`inventoryequipmentid`),
  KEY `inventoryitemid` (`inventoryitemid`),
  CONSTRAINT `equipment_ibfk_1` FOREIGN KEY (`inventoryitemid`) REFERENCES `items` (`inventoryitemid`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `extendedsp`;
CREATE TABLE `extendedsp` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `characterId` int(10) NOT NULL,
  `advancement` tinyint(3) unsigned NOT NULL,
  `points` tinyint(3) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_extendedsp_1` (`characterId`),
  CONSTRAINT `FK_extendedsp_1` FOREIGN KEY (`characterId`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `items`
--

DROP TABLE IF EXISTS `items`;
CREATE TABLE `items` (
  `inventoryitemid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `characterid` int(11) DEFAULT NULL,
  `itemid` int(11) NOT NULL DEFAULT '0',
  `inventorytype` int(11) NOT NULL DEFAULT '0',
  `position` int(11) NOT NULL DEFAULT '0',
  `quantity` int(11) NOT NULL DEFAULT '0',
  `owner` tinytext NOT NULL,
  `petid` int(11) NOT NULL DEFAULT '-1',
  `expiredate` timestamp NULL DEFAULT NULL,
  PRIMARY KEY (`inventoryitemid`),
  KEY `items_ibfk_1` (`characterid`),
  KEY `characterid` (`characterid`),
  KEY `inventorytype` (`inventorytype`),
  KEY `characterid_2` (`characterid`,`inventorytype`),
  CONSTRAINT `items_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `ipbans`
--

DROP TABLE IF EXISTS `ipbans`;
CREATE TABLE `ipbans` (
  `ipbanid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `ip` varchar(40) NOT NULL DEFAULT '',
  PRIMARY KEY (`ipbanid`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `keymap`
--

DROP TABLE IF EXISTS `keymap`;
CREATE TABLE `keymap` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `characterid` int(11) NOT NULL DEFAULT '0',
  `key` int(11) NOT NULL DEFAULT '0',
  `type` int(11) NOT NULL DEFAULT '0',
  `action` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `keymap_ibfk_1` (`characterid`),
  CONSTRAINT `keymap_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `mts_cart`
--

DROP TABLE IF EXISTS `mts_cart`;
CREATE TABLE `mts_cart` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `cid` int(11) NOT NULL,
  `itemid` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `mts_items`
--

DROP TABLE IF EXISTS `mts_items`;
CREATE TABLE `mts_items` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `tab` int(11) NOT NULL DEFAULT '0',
  `type` int(11) NOT NULL DEFAULT '0',
  `itemid` int(10) unsigned NOT NULL DEFAULT '0',
  `quantity` int(11) NOT NULL DEFAULT '1',
  `seller` int(11) NOT NULL DEFAULT '0',
  `price` int(11) NOT NULL DEFAULT '0',
  `bid_incre` int(11) DEFAULT '0',
  `buy_now` int(11) DEFAULT '0',
  `position` int(11) DEFAULT '0',
  `upgradeslots` int(11) DEFAULT '0',
  `level` int(11) DEFAULT '0',
  `str` int(11) DEFAULT '0',
  `dex` int(11) DEFAULT '0',
  `int` int(11) DEFAULT '0',
  `luk` int(11) DEFAULT '0',
  `hp` int(11) DEFAULT '0',
  `mp` int(11) DEFAULT '0',
  `watk` int(11) DEFAULT '0',
  `matk` int(11) DEFAULT '0',
  `wdef` int(11) DEFAULT '0',
  `mdef` int(11) DEFAULT '0',
  `acc` int(11) DEFAULT '0',
  `avoid` int(11) DEFAULT '0',
  `hands` int(11) DEFAULT '0',
  `speed` int(11) DEFAULT '0',
  `jump` int(11) DEFAULT '0',
  `locked` int(11) DEFAULT '0',
  `isequip` int(1) DEFAULT '0',
  `owner` varchar(16) DEFAULT '',
  `sellername` varchar(16) NOT NULL,
  `sell_ends` varchar(16) NOT NULL,
  `transfer` int(2) DEFAULT '0',
  `vicious` int(2) unsigned NOT NULL DEFAULT '0',
  `flag` int(2) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `notes`
--

DROP TABLE IF EXISTS `notes`;
CREATE TABLE `notes` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `to` varchar(45) NOT NULL,
  `from` varchar(45) NOT NULL,
  `message` text NOT NULL,
  `timestamp` bigint(20) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `pets`
--

DROP TABLE IF EXISTS `pets`;
CREATE TABLE `pets` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(13) DEFAULT NULL,
  `level` int(10) unsigned NOT NULL,
  `closeness` int(10) unsigned NOT NULL,
  `fullness` int(10) unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `queststatus`
--

DROP TABLE IF EXISTS `queststatus`;
CREATE TABLE `queststatus` (
  `queststatusid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `characterid` int(11) NOT NULL DEFAULT '0',
  `quest` int(11) NOT NULL DEFAULT '0',
  `status` int(11) NOT NULL DEFAULT '0',
  `time` bigint(20) NOT NULL DEFAULT '0',
  `forfeited` int(11) NOT NULL DEFAULT '0',
  `specialstatus` varchar(10) NOT NULL DEFAULT '',
  PRIMARY KEY (`queststatusid`),
  KEY `characterid` (`characterid`),
  CONSTRAINT `queststatus_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `queststatusmobs`
--

DROP TABLE IF EXISTS `queststatusmobs`;
CREATE TABLE `queststatusmobs` (
  `queststatusmobid` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `queststatusid` int(10) unsigned NOT NULL DEFAULT '0',
  `mob` int(11) NOT NULL DEFAULT '0',
  `count` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`queststatusmobid`),
  KEY `queststatusid` (`queststatusid`),
  CONSTRAINT `queststatusmobs_ibfk_1` FOREIGN KEY (`queststatusid`) REFERENCES `queststatus` (`queststatusid`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `quickslots`;
CREATE TABLE `quickslots` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `characterid` int(10) NOT NULL DEFAULT '0',
  `quickslot` tinyint(1) NOT NULL DEFAULT '0',
  `key` tinyint(2) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `quickslots_ibfk_1` (`characterid`),
  CONSTRAINT `quickslots_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `rings`
--

DROP TABLE IF EXISTS `rings`;
CREATE TABLE `rings` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `ringid` int(10) unsigned NOT NULL,
  `partnerRingId` int(10) unsigned NOT NULL,
  `partnerChrId` int(10) unsigned NOT NULL,
  `partnerName` varchar(45) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `savedlocations`
--

DROP TABLE IF EXISTS `savedlocations`;
CREATE TABLE `savedlocations` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `characterid` int(11) NOT NULL,
  `locationtype` tinyint NOT NULL,
  `map` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `savedlocations_ibfk_1` (`characterid`),
  CONSTRAINT `savedlocations_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `skillmacros`
--

DROP TABLE IF EXISTS `skillmacros`;
CREATE TABLE `skillmacros` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `characterid` int(11) NOT NULL DEFAULT '0',
  `position` tinyint(1) NOT NULL DEFAULT '0',
  `skill1` int(11) NOT NULL DEFAULT '0',
  `skill2` int(11) NOT NULL DEFAULT '0',
  `skill3` int(11) NOT NULL DEFAULT '0',
  `name` varchar(13) DEFAULT NULL,
  `shout` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Definition of table `skills`
--

DROP TABLE IF EXISTS `skills`;
CREATE TABLE `skills` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `skillid` int(11) NOT NULL DEFAULT '0',
  `characterid` int(11) NOT NULL DEFAULT '0',
  `skilllevel` int(11) NOT NULL DEFAULT '0',
  `masterlevel` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `skills_ibfk_1` (`characterid`),
  CONSTRAINT `skills_ibfk_1` FOREIGN KEY (`characterid`) REFERENCES `characters` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Definition of table `wishlist`
--

DROP TABLE IF EXISTS `wishlist`;
CREATE TABLE `wishlist` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `charid` int(11) NOT NULL,
  `sn` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;