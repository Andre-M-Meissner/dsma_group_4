SELECT business_id, date,(CASE
				WHEN conc_text LIKE '%check_in%'
					THEN 'true'
					ELSE 'false'
				END) AS checkin_in_last_tips
                , conc_text
FROM (
	SELECT part2.business_id, part2.date, STRING_AGG(part1.text, '  |||||  ') AS conc_text
	FROM
	(SELECT DISTINCT t99.business_id AS business_id, t99.date AS date, STRING_AGG(t98.text, '  |||  ') AS text
	FROM (SELECT tipstable.business_id, date_trunc('day', generate_series
							( '2016-01-01'::timestamp -- CHANGE DATE HERE 1 ###
							, '2017-12-31'::timestamp -- CHANGE DATE HERE 1 ###
							, '1 day'::interval))::date AS date
						FROM public3.tipstable
						GROUP BY tipstable.business_id) AS t99
	LEFT JOIN
	(SELECT DISTINCT tipstable.business_id, tipstable.text, (tipstable.date)::timestamp AS date
	 FROM public3.tipstable WHERE (tipstable.date)::DATE <= '2017-12-31' AND (tipstable.date)::DATE >= '2016-01-01') AS t98 -- CHANGE DATE HERE 1 ###
	ON t99.business_id = t98.business_id AND t99.date = t98.date::DATE
	GROUP BY t99.business_id, t99.date
	) AS part1
	JOIN
	(SELECT DISTINCT t97.business_id as business_id, t97.date AS date, STRING_AGG(t96.text, '  |||  ') AS text
	FROM (SELECT tipstable.business_id, date_trunc('day', generate_series
							( '2016-01-01'::timestamp -- CHANGE DATE HERE 2 ###
							, '2017-12-31'::timestamp -- CHANGE DATE HERE 2 ###
							, '1 day'::interval))::date AS date
						FROM public3.tipstable
						GROUP BY tipstable.business_id) AS t97
	LEFT JOIN
	(SELECT DISTINCT tipstable.business_id, tipstable.text, (tipstable.date)::timestamp AS date
	 FROM public3.tipstable WHERE (tipstable.date)::DATE <= '2017-12-31' AND (tipstable.date)::DATE >= '2016-01-01') AS t96 -- CHANGE DATE HERE 2 ###
	ON t97.business_id = t96.business_id AND t97.date = t96.date::DATE
	GROUP BY t97.business_id, t97.date
	) AS part2
	ON part1.business_id = part2.business_id, public3.businesstable AS bus99
	WHERE part1.date < part2.date
	AND part2.date >= '2017-01-01'
	AND part1.date >= part2.date-365
	AND bus99.business_id = part1.business_id
	AND bus99.city LIKE 'Las%Vegas'
	AND bus99.state LIKE 'NV'
	AND bus99.categories LIKE '%Restaurants%'
	GROUP BY part2.business_id, part2.date) AS main_query
ORDER BY business_id, date;