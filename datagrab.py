#
# datagrab.py
# ?
#
# Jonatan H Sundqvist
# June 11 2015
#

# TODO | - 
#        - 

# SPEC | -
#        -



from bs4 import BeautifulSoup
from urllib import request


def main():
	html = request.urlopen('http://www.avatar.se/molscript/doc/colour_names.html')
	soup = BeautifulSoup(html)

	with open('colours.txt', 'w', encoding='UTF-8') as f:
		for row in list(soup.body.table.findChildren('tr'))[1:]:
			cells = row.findChildren('td')
			try:
				name, r, g, b = [cells[0].string] + cells[2].string[4:].split(' ')
				f.write('{name: <21}= ({r:.08f}, {g:.08f}, {b:.08f}, 1) :: Colour\n'.format(name=name, r=float(r), g=float(g), b=float(b)))
				# print('{name: <21}= ({r:.08f}, {g:.08f}, {b:.08f}, 1) :: Colour'.format(name=name, r=float(r), g=float(g), b=float(b)))
			except Exception as e:
				print(e)
				print('Ignoring {0}'.format(row))


if __name__ == '__main__':
	main()