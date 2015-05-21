%Gitbit structures
%
%

% Address, GitHub Repo, Create Time, End TIme, Total btc,
% Bounty Status, PayoutAddress Final Transaction ID,
% Short Description, URL of commit, Complete Brag
-record(bounty, {id, name, repo, addr, ctime, endtime, total=0,
				 status, payee, txid, desc, url, brag}).
