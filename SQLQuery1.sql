CREATE DATABASE DiamondsDB;
GO

USE DiamondsDB;
GO

CREATE TABLE Diamonds (
    Id INT,
    Carat FLOAT,
    Cut NVARCHAR(50),
    Color NVARCHAR(10),
    Clarity NVARCHAR(10),
    Depth FLOAT,
    TableColumn FLOAT,
    Price INT,
    X FLOAT,
    Y FLOAT,
    Z NVARCHAR(50)
);
GO

BULK INSERT Diamonds
FROM 'C:\Users\sqladmin\Desktop\diamonds.csv'
WITH 
(
    FIELDTERMINATOR = ',',  -- Określa, że dane są rozdzielone przecinkiem
    ROWTERMINATOR = '\n',   -- Określa, że każdy wiersz kończy się nową linią
    FIRSTROW = 2           -- Pomija pierwszy wiersz, który zawiera nagłówki kolumn
);

SELECT COUNT(*) FROM DIAMONDS_CLEANED



