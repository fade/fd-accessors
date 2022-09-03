UPDATE stock SET is_etf = TRUE
WHERE symbol IN ('ARKF', 'ARKG', 'ARKK', 'ARKQ', 'ARKW', 'ARKX', 'CTRU', 'IZRL', 'PRNT');

select * from stock where is_etf = TRUE;

select * from stock where symbol = 'ARKQ';

-- show me holdings that weren't there yesterday.
select holding_id
from etf_holding
where dt = '2022-06-28'
and holding_id not in (select distinct (holding_id) from etf_holding where dt = '2021-01-25');

select *
from etf_holding
order by etf_id, holding_id, dt;




