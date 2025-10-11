from setuptools import setup, find_packages

setup(
    name="mathai",
    version="0.4.2",
    description="Mathematics solving Ai tailored to NCERT",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    author="educated indians are having a low iq and are good for nothing",
    url="https://github.com/infinity390/mathai4",
    packages=find_packages(),
    install_requires=[
        "lark-parser"
    ],
    python_requires=">=3.7",
)
