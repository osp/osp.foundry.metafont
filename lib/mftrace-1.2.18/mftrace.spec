#-*-RPM-SPEC-*-
Summary: Generate scalable fonts for TeX
Name: mftrace
Version: 1.2.18
Release: 1
URL: http://www.cs.uu.nl/~hanwen/mftrace
Source0: %{name}-%{version}.tar.gz
License: GPL
Group: Applications/Publishing
BuildRoot: %{_tmppath}/%{name}-root
Prereq: tetex python potrace t1utils


%description

Mftrace is a small Python program that lets you trace a @TeX{}
bitmap font into a PFA or PFB font (A PostScript Type1 Scalable Font).
It is licensed under the GNU GPL.

Type1 fonts offer many advantages over bitmaps, as they allow PostScript
files to render correctly on printers with many resolutions. Moreover,
Ghostscript can generate much better PDF, if given scalable fonts.

%prep
%setup -q

%build
./configure --prefix=/usr/
make
make README.txt


%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT/usr/  mandir=$RPM_BUILD_ROOT/%{_mandir} install
%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)

%{_prefix}/bin/gf2pbm
%{_prefix}/bin/mftrace
%{_mandir}/man1/mftrace.1.gz
%{_datadir}/mftrace/afm.*
%{_datadir}/mftrace/tfm.*
%doc README.txt

%changelog
* Tue Feb 19 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
- Initial build.


