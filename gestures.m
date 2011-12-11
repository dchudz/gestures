
#read a couple frames as an example
imagem = aviread("./data/devel01/M_1.avi",1);
imagek = aviread("./data/devel01/K_1.avi",1);


#function to read frames from AVI:
function thisAVI = ReadAVI(type,num)
 data = "./data/devel01/";
 thisAVI = {};
 index=0;
 stop=0;
 while !stop
   index=index+1;
                               #  printf("index: %d \n", index)
                               #  fflush(sdout)
   
   if mod(index,10) == 0
     disp("frame:")
     disp(index)
   endif
   try
     image  = aviread(strcat(data,type,"_",num2str(num),".avi"),index);
     image = imresize(image(:,:,1), .33);
     thisAVI{index} = image(:,:,1);
   catch
     stop = 1;
   end_try_catch
 endwhile
endfunction

#read all frames from all movies in devel01
for i=1:47
  disp("video number:")
  disp(i)
  disp("K")
  Ks{i} = ReadAVI("K",i);
				# disp("M")
				# Ms{i} = ReadAVI("M",i);
endfor


#save
save -mat-binary  "Ks47.mat" Ks


