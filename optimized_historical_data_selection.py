import math
from math import *

class impact_square:
    location1 = 0.0
    location2 = 0.0
    time1 = 1
    time2 = 1
    
input_wea = open("./../Weather_Data.txt","r")
input_sta = open("./../STA_ID.txt","r")
sta_num = int(input_sta.readline())
sta_mp = [0.0 for x in range(sta_num)]
sta_id = [1 for x in range(sta_num)]
for i in range(sta_num):
    line = input_sta.readline()
    temp1,temp2 =[str(x) for x in line.split()]
    sta_mp[i] = float(temp1)
    sta_id[i] = int(temp2)
input_sta.close()
input_imp = open("./../log/impact_range.txt","r")

incident = [impact_square() for x in range(1555)]
incident_num = 0
for line in input_imp.readlines():
    temp1,temp6,temp7,temp2,temp3,temp4,temp5 =[str(x) for x in line.split()]
    incident[incident_num].location1 = temp2
    incident[incident_num].location2 = temp3
    incident[incident_num].time1 = temp4
    incident[incident_num].time2 = temp5
    incident_num+=1
input_imp.close()
output_avail = open("./../log/Availability.txt","w")
print >> output_avail,"weather_id wea_flag mp moy flow delay sky road hour dow"
output_stat = open("./../log/Statis_Avail.txt","w")
output_adverse = open("./../log/adverse_weather_traffic.txt","w")
day_of_month = [0,31,59,90,120,151,181,212,243,273,304,334]
print >> output_adverse,"mp weather speed flow delay sky road hour dow"
for i in range(sta_num):
	input_target = open("./../output/new_file/update_N"+str(sta_id[i])+"_VAR.txt","r")
	print "Station",str(sta_id[i])," is opened"
	tar_flow = [0.0 for x in range(800000)]
	tar_delay = [0.0 for x in range(800000)]
	tar_speed = [0.0 for x in range(800000)]
	for target_line in input_target.readlines():
		temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13 = [str(x) for x in target_line.split()]
		tar_day = int(temp1)
		tar_hour = int(temp2)
		tar_minute = int(temp3)
		tar_minute_of_year = 24*60*(tar_day-1)+60*tar_hour+tar_minute
		tar_flow[tar_minute_of_year] = float(temp13)
		tar_delay[tar_minute_of_year] = float(temp10)
		tar_speed[tar_minute_of_year] = float(temp5)
	input_target.close()
	input_wea = open("./../Weather_Data.txt","r")
	weather_num = 0
	for line in input_wea.readlines():
	    avail_num = 0
	    year = int(line[0:2])
	    month = int(line[2:4])
	    day = int(line[4:6])
	    hour = int(line[6:8])
	    minute = int(line[8:10])
	    seg_id = int(line[10:12])
	    road_cond = int(line[12:13])
	    sky_cond = int(line[13:14])
	    weather_num+=1
	    #print i, weather_num
	    #print year,month,day,hour,minute,seg_id,sky_cond,road_cond
	    day_of_year = day_of_month[month-1]+day-1
	    minute_of_year = day_of_year*24*60+hour*60+minute
#	    if year==13:
            print >>output_adverse, sta_mp[i],weather_num,tar_speed[minute_of_year], tar_flow[minute_of_year],tar_delay[minute_of_year],sky_cond,road_cond,hour,day_of_year%7+1
	    #print day_of_year,minute_of_year
	    minute_of_week = minute_of_year%(7*24*60)
	    #print minute_of_week
	    target_minute = minute_of_week#+304*24*60	 
	    while target_minute<=485*24*60:
		#check target_minute at all the station
		avail_num=0
		#print weather_num,target_minute
		if (target_minute!=minute_of_year)or(target_minute==minute_of_year):        
#			for i in range(sta_num):
			avail = 1
		    	for j in range(incident_num):
				if (sta_mp[i]<=incident[j].location2)and(sta_mp[i]>=incident[j].location1)and(target_minute<=incident[j].time2)and(target_minute>=incident[j].time1):
					avail = 0
					break
			if avail ==1:
				#print weather_num, line
				avail_num+=1
				if (target_minute!=minute_of_year)and(target_minute<=365*1440):				
					print >> output_avail,weather_num,"0", sta_mp[i],target_minute, tar_flow[target_minute],tar_delay[target_minute], sky_cond,road_cond,hour,day_of_year%7+1
				else:
					if (target_minute==minute_of_year):
						print >> output_avail,weather_num,"1", sta_mp[i],target_minute, tar_flow[target_minute],tar_delay[target_minute], sky_cond,road_cond,hour,day_of_year%7+1
		target_minute+=7*24*60

	input_wea.close()        
output_avail.close()
output_adverse.close()
output_stat.close()
