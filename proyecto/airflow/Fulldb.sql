-- MySQL dump 10.13  Distrib 8.0.19, for Win64 (x86_64)
--
-- Host: localhost    Database: Covid
-- ------------------------------------------------------
-- Server version	5.7.32

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Temporary view structure for view `ALLSumarizado`
--

DROP TABLE IF EXISTS `ALLSumarizado`;
/*!50001 DROP VIEW IF EXISTS `ALLSumarizado`*/;
SET @saved_cs_client     = @@character_set_client;
/*!50503 SET character_set_client = utf8mb4 */;
/*!50001 CREATE VIEW `ALLSumarizado` AS SELECT 
 1 AS `Pais/Region`,
 1 AS `Confirmados`,
 1 AS `Muertes`,
 1 AS `Recuperados`*/;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `Cases`
--

DROP TABLE IF EXISTS `Cases`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Cases` (
  `Lat` double DEFAULT NULL,
  `Long` double DEFAULT NULL,
  `State` varchar(45) DEFAULT NULL,
  `Country` varchar(45) DEFAULT NULL,
  `RegDate` datetime DEFAULT NULL,
  `Count` int(11) DEFAULT NULL,
  `Type` varchar(1) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Temporary view structure for view `Totals`
--

DROP TABLE IF EXISTS `Totals`;
/*!50001 DROP VIEW IF EXISTS `Totals`*/;
SET @saved_cs_client     = @@character_set_client;
/*!50503 SET character_set_client = utf8mb4 */;
/*!50001 CREATE VIEW `Totals` AS SELECT 
 1 AS `Type`,
 1 AS `Lat`,
 1 AS `Long`,
 1 AS `Country`,
 1 AS `total`*/;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `Types`
--

DROP TABLE IF EXISTS `Types`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Types` (
  `Type` varchar(1) NOT NULL,
  `Descripcion` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`Type`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping events for database 'Covid'
--

--
-- Dumping routines for database 'Covid'
--

--
-- Final view structure for view `ALLSumarizado`
--

/*!50001 DROP VIEW IF EXISTS `ALLSumarizado`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb4 */;
/*!50001 SET character_set_results     = utf8mb4 */;
/*!50001 SET collation_connection      = utf8mb4_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`%` SQL SECURITY DEFINER */
/*!50001 VIEW `ALLSumarizado` AS select `Cases`.`Country` AS `Pais/Region`,max((case when (`Cases`.`Type` = 'C') then `Cases`.`Count` else 0 end)) AS `Confirmados`,max((case when (`Cases`.`Type` = 'D') then `Cases`.`Count` else 0 end)) AS `Muertes`,max((case when (`Cases`.`Type` = 'R') then `Cases`.`Count` else 0 end)) AS `Recuperados` from `Cases` group by `Cases`.`Country` */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;

--
-- Final view structure for view `Totals`
--

/*!50001 DROP VIEW IF EXISTS `Totals`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb4 */;
/*!50001 SET character_set_results     = utf8mb4 */;
/*!50001 SET collation_connection      = utf8mb4_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`%` SQL SECURITY DEFINER */
/*!50001 VIEW `Totals` AS select `Cases`.`Type` AS `Type`,`Cases`.`Lat` AS `Lat`,`Cases`.`Long` AS `Long`,`Cases`.`Country` AS `Country`,ifnull(sum(`Cases`.`Count`),0) AS `total` from `Cases` group by `Cases`.`Lat`,`Cases`.`Long`,`Cases`.`Type`,`Cases`.`Country` */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2020-12-11  0:33:55
