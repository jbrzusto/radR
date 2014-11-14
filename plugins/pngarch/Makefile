ocr.so: ocr.c
	gcc -O3 -c -fPIC -o ocr.o -I /home/src/ocrad-0.24/ -I /usr/lib/R/include -I ../../include ocr.c -L /home/src/ocrad-0.24/
	g++ -shared -fPIC -o ocr.so  ocr.o -I /home/src/ocrad-0.24/ -I /usr/lib/R/include -I ../../include -L /home/src/ocrad-0.24/ -locrad  -L /usr/lib/R -lR
