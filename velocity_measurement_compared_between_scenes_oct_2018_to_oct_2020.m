% % % This is the codes that have been modified from Imgraft package which can be found here: http://imgraft.glaciology.net/home % % %
% % % This one will extract data from imagery pair between Scenes 1 with 2/ two
% with 3/ 3 with 4 and 4 with 5 and so on. 
% % % Thatsanee Sanouban % % %

% % set working folder to the folder that contains all preprocessed imagery 2018 - 2020
  % This imagery is consider as good imagery already
  % Note that the work folder here is in the personal labtop 
  
workfolder='C:\Users\Advice\OneDrive - Victoria University of Wellington - STUDENT\GISC 511\ImGraft\Tasman_Single_Band_Good_IM_2018-2020_UTM';
outputfolder='C:\Users\Advice\OneDrive - Victoria University of Wellington - STUDENT\GISC 511\ImGraft\Velocity_Tiff (1-2, 2-3..)';

% % list all the file names in the folder.

filenames = ls(strcat(workfolder, '\', '*.tif'));

% % Get date data from all imagery in that folder
  % NOTES: I will use similar technique here.
files = dir(strcat(workfolder, '\', '*.tif'));

% % create an emty 0 array  for storing the date number data

date_num = zeros(1, length(files));

% % extract time data from all imagery using for loop.

for i = 1:length(files)
   date = regexp(files(i).name,'\.','split');
   date = date{1};
   y = str2double(date(1:4));
   m = str2double(date(5:6));
   d = str2double(date(7:8));
     % append the date data in to newly created array 
     % im_dates(i) = datetime(y,m,d); % this im_dates contain YYYYMMDD format
   date_num(i) = datenum(y,m,d); % this date_num array contains date in number format which can be calculate (minus) and return the number of days.
       
end

% % Use for loop to find the displacement velocity and save it into a
  % georeference file every time it loops by paring image i with 1 + i. 

for i = 1:(length(filenames)-1) % we will loop from 1 to the second last number of imager (Pairing 1 and 1 + i)
   
   % % the total number of pair equal to length(filenames)-1.
     % In this case we will pair image 1 with image 1 + i which will be
     % assigned to j instead 
      
   j = 1 + i;
    
    % % Read imagery which is in geotiff format.
      % We will read image 1 and 1 + i every forloop until the last i
      % the last i is the second last imagery and will  result the last j
      % which will be the last imagery in the folder
    
   [A,x,y,Ia]=geoimread(fullfile(workfolder,filenames(i,1:12)));
   [B,xb,yb,Ib]=geoimread(fullfile(workfolder,filenames(j,1:12)));
    
      
   deltax=x(2)-x(1);%m/pixel
   deltay=y(2)-y(1);%m/pixel

   % % make regular grid of points to track:
    
   [pu,pv]=meshgrid(0:10:size(A,2), 0:10:size(A,1)); %pixel coordinated

   % % obtain corresponding map coordinates of pixel coordinates
    
   px=interp1(x,pu); py=interp1(y,pv);

   % % now calculate the correlation between a pair of images

   [du,dv,C,Cnoise,pu,pv]=templatematch(A,B,pu,pv,'showprogress',true,'method','oc');
   close all
      % note: du,dv: displacement of each point in pu,pv. [A(pu,pv) has moved to B(pu+du,pv+dv)]
      % pu,pv: actual pixel centers of templates in A (may differ slightly from inputs because of rounding).
    
    
    
   % % extract time interval from imagery name which is the number of days
    
   num_days = date_num(j) - date_num(i);
    
   % % extract time for a pair of image 
    
   V = (((du*deltax)+((dv*1i)*deltay))/num_days)*365; % displacment velocity per year
   Vn = abs(V); % absolute number of velocity
    
   % % Create the referencing matrix (R) % for the Tasman glacier for writing Geotiff file.

   R = georefcells([-43.662233 -43.5986],[170.184486 170.226867], size(Vn), ...
                       'ColumnsStartFrom', 'north');

   % % write Geotiff file every time it loops. 
    
   geotiffwrite(strcat(outputfolder,'\', filenames(i,1:8), '_', filenames(j,1:8), '.tif'),Vn,R)
   
end
    
     



