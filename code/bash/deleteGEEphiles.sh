earthengine task list  > info




tasks=$(cat taskinfo | grep READY | awk '{print $1}')

for i in $tasks;do    
    echo $i 
    earthengine task cancel $i 
done


earthengine task list  > info
tasks=$(cat info | grep RUNNING | awk '{print $1}')

for i in $tasks;do    
    echo $i 
    earthengine task cancel $i 
done




