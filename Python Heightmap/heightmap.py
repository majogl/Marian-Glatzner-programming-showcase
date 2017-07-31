from random import randrange as rr
from pprint import pprint
import random
import png

random.seed()
MAX_H = 255

def generate_heightmap(SIZE,coords=[]):

	def in_bounds(n):
		return n in range(mw+1)

	def square_step():
		while new_squares != set():
			x,y,r = new_squares.pop()
			if r <= 0:
				return
			if val[x][y] is None:
				val[x][y] = int((val[x+r][y+r] + val[x+r][y-r] + val[x-r][y+r] + val[x-r][y-r])/4) + (rr(2*r) - r)//2
			if val[x][y] < 0:
				val[x][y] = 0
			elif val[x][y] > 255:
				val[x][y] = 255
			if in_bounds(x+r):
				new_diamonds.add((x+r,y,r))
			if in_bounds(x-r):
				new_diamonds.add((x-r,y,r))
			if in_bounds(y+r):
				new_diamonds.add((x,y+r,r))
			if in_bounds(y-r):
				new_diamonds.add((x,y-r,r))
		print("Radius: {} Diamond operations to be calculated: {}".format(r,len(new_diamonds)))
		diamond_step()

	def diamond_step():
		while new_diamonds != set():
			x,y,r = new_diamonds.pop()
			temp = []
			if in_bounds(x+r):
				temp.append(val[x+r][y])
			if in_bounds(x-r):
				temp.append(val[x-r][y])
			if in_bounds(y+r):
				temp.append(val[x][y+r])
			if in_bounds(y-r):
				temp.append(val[x][y-r])
			if val[x][y] is None:
				val[x][y] = int(sum(temp)/len(temp)) + (rr(2*r) - r)//2
			if val[x][y] < 0:
				val[x][y] = rr(10)
			elif val[x][y] > 255:
				val[x][y] = 255 - rr(10)

			r = r//2
			if r > 0:
				if in_bounds(x+r) and in_bounds(y+r):
					new_squares.add((x+r,y+r,r))
				if in_bounds(x+r) and in_bounds(y-r):
					new_squares.add((x+r,y-r,r))
				if in_bounds(x-r) and in_bounds(y+r):
					new_squares.add((x-r,y+r,r))
				if in_bounds(x-r) and in_bounds(y-r):
					new_squares.add((x-r,y-r,r))
		print("Radius: {} Square operations to be calculated: {}".format(r,len(new_squares)))
		if len(new_squares) > 0:
			square_step()

	def smooth():
		print('Smoothening heightmap...')
		temp = range(1,len(val)-1)
		for i in temp:
			for j in temp:
				val[i][j] = sum([val[x][y] for x in range(i-1,i+2) for y in range(j-1,j+2)])//9

	mw,size = 2**SIZE,2**(SIZE-1)
	val = [[None for x in range(mw+1)] for y in range(mw+1)]
	val[0][0],val[0][mw],val[mw][0],val[mw][mw] = rr(MAX_H),rr(MAX_H),rr(MAX_H),rr(MAX_H)
	new_squares,new_diamonds = {(size,size,size)}, set()
	square_step()
	smooth()
	return val

print('Generating heightmap matrix')
x = generate_heightmap(8)
print('Exporting heightmap as png')
png.from_array(x, 'L').save("heightmap.png")
