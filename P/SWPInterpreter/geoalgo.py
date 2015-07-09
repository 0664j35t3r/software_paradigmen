#1 /usr/bin/python3
__author__ = 'jester'


a = [(1,2),(3,4), (5,6), (7,8), (9,10), (11,12)]
finish = []

finish.append(a[0])

for points in a:
    if points[0] > a.next()[0]:
        finish.append(points)
    else:
        finish.append(a.next())


print finish
