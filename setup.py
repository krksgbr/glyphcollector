from setuptools import setup


plist = dict(
    CFBundleName="GlyphCollector",
    CFBunderPackageType="APPL",
    LSMinimumSystemVersion="10.9.0",
    CFBundleShortVersionString="0.9",
    CFBundleVersion="beta",
    CFBundleIconFile="GlyphCollector.icns",
    NSHumanReadableCopyright="Copyright by Gabor Kerekes.",
)

APP = ['main.py']

setup(
    app=APP,
    name='GlyphCollector',
    version='0.9 beta',
    setup_requires=['py2app'],

    options=dict( py2app=dict(
        iconfile='GlyphCollector.icns',
        argv_emulation=True,
        excludes=[
            "matplotlib",
            "scipy",
        ],
    ) )
)