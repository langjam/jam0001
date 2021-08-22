from setuptools import setup

with open("requirements.txt") as fp:
    requirements = [l.strip() for l in fp.readlines()]

setup(
    name="arrdem.lilith",
    # Package metadata
    version="0.0.0",
    license="MIT",
    # Package setup
    package_dir={"": "src/python"},
    packages=[
        "lilith",
    ],
    package_data={"": [
        "src/python/lilith/*.lark",
        "src/python/lilith/*.lil",
    ]},
    include_package_data=True,
    install_requires=requirements,
    entry_points={
        "console_scripts": [
            "lil = lilith.__main__:main",
        ],
    },
)
