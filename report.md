Εκτελέσιμο

Το αρχείο Demo είναι ένα εκτελέσιμο για linux. Το Demo δέχεται σε μια γραμμή το ελάχιστο και μέγιστο γράμμα που μπορεί να αρχίζει
το όνομα του επιστήμονα και το ελάχιστο και μέγιστο αριθμό βραβείων που μπορεί να έχει, με αυτή τη σειρά, και τυπώνει τα
ζεύγη επιστημόνων που πληρούν αυτά τα κριτήρια και έχουν ομοιότητα στην εκπαίδευση >20%.

Παράδειγμα

Τα δέντρα αποθηκεύουν πληροφορία σε μορφή Point. Ένα Point περιέχει δυο νούμερα x και y που δηλώνουν το πρώτο γράμμα του ονόματος
και τον αριθμό βραβείων και μια λίστα stuff με δείκτες προς τους επιστήμονες που περιγραφει.
Επίσης καταγραφουν σε ποια από τις δυο διαστάσεις πρέπει να συγκριθούν μεταξύ τους.

Τα τέσσερα είδη δέντρων (Kdtree,Qtree,RangeTree,R-Tree) συγκριθηκαν ως προς την ταχύτητα στο range querry.
Το Kdtree αποδείχτηκε το πιο γρήγορο και χρησιμοποιήθηκε στο Demo.
Οι δείκτες προς τους επιστήμονες αντιστοιχίζονται σε προϋπολογισμένες υπογραφές LSH και βρίσκονται τα ζεύγη που
καταλήγουν στον ίδιο κουβα σε τουλάχιστον ένα band. Στη συνεχεια φιλτράρονται έτσι ώστε το jaccard similarity τους
να είναι >20%. Αυτό γίνεται με προϋπολογισμένα σύνολα από hash λέξεων τις εκπαίδευσης.
Τέλος τυπώνεται το επώνυμο και αριθμός βραβείων για κάθε επιστήμονα του ζεύγους.

Perigrafi dentron

Kathe dentro exei mia synartisei kataskeyis pou dexetai mia lista me Point kai ftiaxnei ena dentro pou ta periexei
kai mia synartisi gia range querry pou dexetai dio Point pou anaparistoun to elaxisto kai megisto simeio tou range
kai to dentro kai epistrefei ola ta Point sto dentro pou perilambanontai sto range. Ayta ta dio Point tha anaferontai os
low kai high antistoixa.

Kdtree

Kataskeyi : Oi times apothikeyonati sta fylla. Ta Point xorizontai aristera kai dexia me basi to an einai 
mikrotera / isa i megalitera apo to meso oro ton Point tis listas. O kombos apokta ti timi tou mesou orou ton Point.
Sti sinexeia gia kathe kateytinsi h kataskeyi epanalambanetai me ti sigrisi na ginetai se diaforetiki diastasi apo to proigoumeno epipedo.
Otan menei mono ena Point sti list dimiourgitai filo.

Anazitisi : I anazitisi se filo epistrefei ti timi tou filou an einai entos tou range. I anazitisi se kombo epistrefei tin enosi
tis anazitisis sto aristero paidi an o kombos einai megaliteros tou low kai sto dexi an einai mikroteros tou high.
I diastasi stin opoia ginetai i sigrisi allazei kathe epipedo.

QuadTree

Kataskeyi : Ta Point xorizontai sta 4 me basi se poio tetartimorio briskontai se sxesi me to meso Point kai ena ypodentro 
dimiourgitai gia kathe tetartimorio.

Anazitisi : Epistrefetai i enosi ton anazitiseon sta ypodentra pou antistoixoun se ola ta tetartimoria pou brisketai
kapoia gonia tou orthogoniou pou orizei to range.

RangeTree 

Kataskeyi : Sto dentro tis protis diastasis kathe kombos xorizei ta Point se dexia kai aristera ypodentra me basi sigrisi me to
meso Point stin proti diastasi kai episis pernaei ola ta Point se ena katheto ypodentro gia sygkrisi stin alli diastasi. To dentro 
mporei na min einai isozigismeno se kapoies periptoseis logo tou oti i sigrisi ginetai me basi to meso oro pou borei na min 
diairei ta simeia akribos sta dio. Peiramatika ayto den exei arnitikes epiptoseis stin apodosi.
Otan ola ta Point tis listas exoun tin idia timi sti proti diastasi tote mono to katheto ypodentro dimiourgitai.
Sto katheto ipodentro kathe kombos xorizei ta Point se dexia kai aristera ypodentra me basi sigrisi me to
meso Point stin deyteri diastasi.
Otan ola ta Point tis listas exoun tin idia timi sti deyteri diastasi tote dimiourgitai filo me ola ta Point.

Anazitisi : Anafero parakato simainei epistrefo to apotelesma tis anazitisis sto katheto ipodentro.
Arxika brisketai o anoteros kombos pou einai entos tou range, pou tha einai o elaxistos koinos apogonos
ton simeion entos tou range. Apo ekei i anazitisi xorizetai stin aristeri pleyra, opou to dexi ypodentro anaferetai kathe fora
pou to low einai mikrotero apo ton kombo, kai tin dexia pleyra opou to simetriko simbenei.
Otan mia apo tis dio pleyres ftasei se kombo me mono katheto ypodentro ton anaferei an einai entos tou range.
Sta katheta ypodentra i anazitisi ginetai me paromio tropo alla mono me aristero kai dexio ipodentro kai me sigrisi stin alli diastasi.

R-Tree

Kataskeyi : O kombos pairnei os timi to MBR pou perilambanei ola ta Point tis listas.
Ta point taxinomountai me basi tin diastasi stin opoia exoun tis perisoteres monadikes times.
Epeita xorizontai se osa ypodentra oso to elaxisto tou arithmou Point kai tou fanout. Otan mono ena Point menei
dimiourgitai filo.

Anazitisi : Epistrefetai i enosi tis anazitisis se ola ta ypodentra pou exoun MBR pou temnei to range.

