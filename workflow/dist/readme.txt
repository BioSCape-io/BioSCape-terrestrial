Instructions:

Copy the template.csv in the main iNat_bulk_uploader folder to another location.
Open the csv file and replace the dummy records with the details for the records you want to upload. You must save the csv file as Comma Delimited.
Ensure that the correct format is used for each field. Each row will be uploaded as a separate observation.
The program uses the header name of the csv file to assign data to a particular field.
The program skips the first row of the csv file, which should be kept as the header.
You can add observations with no media, but you must enter "none" in the "media_name_1" column or else it will assume that the media was accidentally left out and abort that record.
If you can to abort the program at a certain point in the csv file you must write "ABORT" in the ID column.

Next create a folder and copy all your photos that have been linked from the csv file. Ensure that all photos linked are present and correctly spelt in the csv file.

Once all records have been added to the csv file and the photos are all present a single folder, you can run the executeable (iNat_bulk_uploader.exe).
Enter your iNaturalist username and password (as you would for the website).
Select your csv file and the location of your photo folder, then submit the data.
The program will save your username, folder and file selection for next time. Your password is not stored or shared, but it is required to create a token to connect with the API.

If your login details are correct, the program will begin uploading the records. As records start the upload process the program will print out the 'id' field from the csv.
Please be patient and do not close the program until it says the upload has been completed. 
Once complete, iNaturalist may take some time before the records show on your account. Please check that everything was correctly imported by opening the log.csv in the installation folder. 
The log file will record any errors or photos that failed to upload, it will also be replaced when the program is rerun, so save it elsewhere if necessary.

It is recommended that you first test the bulk uploader on a small number of records to see if it is working before attempting a large upload.

Note that no functionality for adding observations to projects is currently supported, hopefully this will be made possible in the future.

Template.csv Field descriptions: You can re-arrange the fields as desired, but ensure that the text of the headers remain the same.
Fields:
	id: unique id in spreadsheet used to track progress during submission.
	taxon_name: The name of the species/taxon being submitted (must be spelt correctly or else is added as a placeholder)
	date_obs: the date that the observation was observed(DD/MM/YYYY)
	time_zone: (Johannesburg as default), see https://stackoverflow.com/questions/13866926/is-there-a-list-of-pytz-timezones for more
	description: The description field the observation to record any additional notes (as a string)
	tag_list: A list of iNatualist tags separated by a comma
	latitude: decimal latitude, must include the sign but with no foerighn symbols (e.g. -32.9512 )	
	longitude:  decimal longitude must include the sign but with no foerighn symbols 
	pos_acc: the positional accurracy of the coordinates in meters, ie. the radius of uncertainty (3 m for GPS)
	location: name of place, province, country that the observation was recorded
	geoprivacy: either open or obscured, determines whether the location will be obscured on iNaturalist
	field_id1: Observation field id (e.g. 7498 ; see https://www.inaturalist.org/observation_fields/7498)
	field_value1: the value of the observation field (note that some fields only take discrete values)
	field_id2: see above (a second field id)
	field_value2: see above (a second field value)
	media_name_1: the file name of the first photo or sound, exclude all spaces and including the ext. (eg. image1.jpg)
	media_name_2:"
	media_name_3:"
	media_name_4:"
	media_name_5:"
	media_name_6:"
	media_name_7:"
	media_name_8:"
	media_name_9:"
	media_name_10:"



Please report any issues to arebelo23@gmail.com